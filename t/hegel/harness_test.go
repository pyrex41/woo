package hegeltest

import (
	"bytes"
	"context"
	"crypto/rand"
	"fmt"
	"net"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"syscall"
	"testing"
	"time"
)

type fixtureSpec struct {
	name       string
	command    []string
	readyPath  string
	startupTTL time.Duration
	started    func(int)
}

type boundedLog struct {
	mu        sync.Mutex
	b         bytes.Buffer
	file      *os.File
	fileBytes int
	writeErr  error
}

func (l *boundedLog) Write(p []byte) (int, error) {
	l.mu.Lock()
	defer l.mu.Unlock()
	if remaining := 32*1024 - l.b.Len(); remaining > 0 {
		l.b.Write(p[:min(len(p), remaining)])
	}
	if l.file != nil && l.fileBytes < 256*1024 {
		remaining := min(len(p), 256*1024-l.fileBytes)
		written, err := l.file.Write(p[:remaining])
		l.fileBytes += written
		if err != nil && l.writeErr == nil {
			l.writeErr = err
		}
	}
	return len(p), nil
}

func (l *boundedLog) String() string {
	l.mu.Lock()
	defer l.mu.Unlock()
	return l.b.String()
}

var oracleBuildOnce sync.Once
var oracleBuildErr error
var oracleBuildOutput string

func oracleBinary(t *testing.T, root string) string {
	t.Helper()
	if binary := os.Getenv("WOO_HEGEL_ORACLE_BIN"); binary != "" {
		return binary
	}
	manifest := filepath.Join(root, "t", "hegel", "oracle", "Cargo.toml")
	oracleBuildOnce.Do(func() {
		ctx, cancel := context.WithTimeout(context.Background(), 6*time.Minute)
		defer cancel()
		cmd := exec.CommandContext(ctx, "cargo", "build", "--locked", "--manifest-path", manifest)
		cmd.Dir = root
		output, err := cmd.CombinedOutput()
		oracleBuildOutput = string(output)
		if ctx.Err() != nil {
			oracleBuildErr = fmt.Errorf("cargo build timed out: %w", ctx.Err())
		} else {
			oracleBuildErr = err
		}
	})
	if oracleBuildErr != nil {
		t.Fatalf("build axum oracle: %v\n%s", oracleBuildErr, oracleBuildOutput)
	}
	return filepath.Join(root, "t", "hegel", "oracle", "target", "debug", "woo-hegel-oracle")
}

// startFixture owns the complete child lifecycle. Readiness is an HTTP
// identity check, so a stale process accepting the selected port cannot make a
// test pass.
func startFixture(t *testing.T, spec fixtureSpec, requestedPort string) (string, error) {
	t.Helper()
	root, err := filepath.Abs(filepath.Join("..", ".."))
	if err != nil {
		return "", err
	}
	port, err := fixturePort(requestedPort)
	if err != nil {
		return "", err
	}
	nonceBytes := make([]byte, 18)
	if _, err := rand.Read(nonceBytes); err != nil {
		return "", err
	}
	nonce := fmt.Sprintf("%x", nonceBytes)
	command := make([]string, len(spec.command))
	for i, argument := range spec.command {
		command[i] = strings.ReplaceAll(argument, "$WOO_HEGEL_PORT", strconv.Itoa(port))
	}
	cmd := exec.Command(command[0], command[1:]...)
	cmd.Dir = root
	cmd.Env = append(os.Environ(),
		"WOO_HEGEL_PORT="+strconv.Itoa(port),
		"WOO_HEGEL_READY_NONCE="+nonce)
	// Managed gates own the enclosing process group so stage failure/timeout
	// also removes fixtures. Standalone Hegel keeps its per-fixture groups.
	ownGroup := os.Getenv("WOO_COMPAT_FIXTURE_GROUP") != "owned-stage"
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: ownGroup}
	var metricLog *os.File
	if path := os.Getenv("WOO_LEGACY_MEMORY_LOG"); path != "" {
		metricLog, err = os.OpenFile(path, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0600)
		if err != nil {
			return "", fmt.Errorf("open bounded memory diagnostic log: %w", err)
		}
	}
	fileBytes := 0
	if metricLog != nil {
		if info, statErr := metricLog.Stat(); statErr == nil {
			fileBytes = int(info.Size())
		}
	}
	log := &boundedLog{file: metricLog, fileBytes: fileBytes}
	cmd.Stdout, cmd.Stderr = log, log
	if err := cmd.Start(); err != nil {
		if metricLog != nil {
			_ = metricLog.Close()
		}
		return "", fmt.Errorf("start %s: %w", spec.name, err)
	}
	if spec.started != nil {
		spec.started(cmd.Process.Pid)
	}
	exited := make(chan struct{})
	var exitErr error
	go func() {
		exitErr = cmd.Wait()
		close(exited)
	}()
	cleanup := func() {
		if cmd.Process != nil {
			if ownGroup {
				_ = syscall.Kill(-cmd.Process.Pid, syscall.SIGKILL)
			}
			_ = cmd.Process.Kill()
		}
		select {
		case <-exited:
		case <-time.After(3 * time.Second):
			t.Errorf("%s did not exit after kill; output: %s", spec.name, log.String())
		}
		if metricLog != nil {
			_ = metricLog.Close()
			metricLog = nil
		}
		if log.writeErr != nil {
			t.Errorf("%s memory diagnostic log write failed: %v", spec.name, log.writeErr)
		}
	}
	t.Cleanup(func() {
		if t.Failed() {
			t.Logf("%s output:\n%s", spec.name, log.String())
		}
		cleanup()
	})
	addr := net.JoinHostPort("127.0.0.1", strconv.Itoa(port))
	deadline := time.Now().Add(spec.startupTTL)
	for time.Now().Before(deadline) {
		select {
		case <-exited:
			return "", fmt.Errorf("%s exited during startup: %v\n%s", spec.name, exitErr, log.String())
		default:
		}
		client := &http.Client{Timeout: 150 * time.Millisecond}
		response, requestErr := client.Get("http://" + addr + spec.readyPath)
		if requestErr == nil {
			body := new(bytes.Buffer)
			_, copyErr := body.ReadFrom(response.Body)
			response.Body.Close()
			if copyErr == nil && response.StatusCode == http.StatusOK && body.String() == nonce {
				return addr, nil
			}
		}
		time.Sleep(50 * time.Millisecond)
	}
	return "", fmt.Errorf("%s did not become ready on %s within %s\n%s", spec.name, addr, spec.startupTTL, log.String())
}

func fixturePort(requested string) (int, error) {
	if requested != "" {
		port, err := strconv.Atoi(requested)
		if err != nil || port < 1 || port > 65535 {
			return 0, fmt.Errorf("invalid fixture port %q", requested)
		}
		listener, err := net.Listen("tcp", "127.0.0.1:"+strconv.Itoa(port))
		if err != nil {
			return 0, fmt.Errorf("fixture port %d is unavailable: %w", port, err)
		}
		return port, listener.Close()
	}
	listener, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		return 0, err
	}
	port := listener.Addr().(*net.TCPAddr).Port
	return port, listener.Close()
}

func startWoo(t *testing.T) string {
	lisp := os.Getenv("WOO_HEGEL_LISP")
	if lisp == "" {
		lisp = "sbcl"
	}
	command := []string{lisp, "--script", "t/hegel/server.lisp"}
	if lisp == "ros" {
		command = []string{"ros", "-e", `(load "t/hegel/server.lisp")`}
	}
	addr, err := startFixture(t, fixtureSpec{
		name:       "Woo",
		command:    command,
		readyPath:  "/.woo-test-ready",
		startupTTL: 120 * time.Second,
	}, os.Getenv("WOO_HEGEL_PORT"))
	if err != nil {
		t.Fatal(err)
	}
	return addr
}

func startOracle(t *testing.T) string {
	root, err := filepath.Abs(filepath.Join("..", ".."))
	if err != nil {
		t.Fatal(err)
	}
	binary := oracleBinary(t, root)
	if _, err := os.Stat(binary); err != nil {
		t.Fatalf("oracle binary is unavailable: %v", err)
	}
	addr, err := startFixture(t, fixtureSpec{
		name:       "Rust oracle",
		command:    []string{binary, "--port", "$WOO_HEGEL_PORT"},
		readyPath:  "/.woo-test-ready",
		startupTTL: 30 * time.Second,
	}, os.Getenv("WOO_HEGEL_ORACLE_PORT"))
	if err != nil {
		t.Fatal(err)
	}
	return addr
}

func TestFixtureRejectsOccupiedPort(t *testing.T) {
	listener, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	defer listener.Close()
	port := strconv.Itoa(listener.Addr().(*net.TCPAddr).Port)
	_, err = startFixture(t, fixtureSpec{
		name:       "Woo",
		command:    []string{"definitely-not-a-fixture"},
		readyPath:  "/.woo-test-ready",
		startupTTL: 100 * time.Millisecond,
	}, port)
	if err == nil {
		t.Fatal("occupied fixture port was accepted")
	}
	if !bytes.Contains([]byte(err.Error()), []byte("port")) {
		t.Fatalf("unexpected occupied port error: %v", err)
	}
}

func TestFixtureRejectsChildExitBeforeReadiness(t *testing.T) {
	_, err := startFixture(t, fixtureSpec{
		name:       "failing fixture",
		command:    []string{"/bin/sh", "-c", "exit 7"},
		readyPath:  "/.woo-test-ready",
		startupTTL: 10 * time.Second,
	}, "")
	if err == nil {
		t.Fatal("fixture that exited before readiness was accepted")
	}
	if !strings.Contains(err.Error(), "exited during startup") {
		t.Fatalf("unexpected child exit error: %v", err)
	}
}
