package hegeltest

import (
	"bufio"
	"bytes"
	"context"
	"crypto/sha1"
	"crypto/tls"
	"encoding/base64"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"sync"
	"syscall"
	"testing"
	"time"

	"golang.org/x/net/http2"
	"hegel.dev/go/hegel"
)

type parityResult struct {
	status int
	proto  int
	body   []byte
}

var oracleBuildOnce sync.Once
var oracleBuildErr error
var oracleBuildOutput string

func startOracle(t *testing.T) string {
	t.Helper()
	root, err := filepath.Abs(filepath.Join("..", ".."))
	if err != nil {
		t.Fatal(err)
	}
	manifest := filepath.Join(root, "t", "hegel", "oracle", "Cargo.toml")
	binary := os.Getenv("WOO_HEGEL_ORACLE_BIN")
	if binary == "" {
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
		binary = filepath.Join(root, "t", "hegel", "oracle", "target", "debug", "woo-hegel-oracle")
	}
	listener, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	port := listener.Addr().(*net.TCPAddr).Port
	listener.Close()
	cmd := exec.Command(binary, "--port", strconv.Itoa(port))
	cmd.Dir = root
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	log := &boundedLog{}
	cmd.Stdout, cmd.Stderr = log, log
	if err := cmd.Start(); err != nil {
		t.Fatalf("start axum oracle: %v", err)
	}
	exited := make(chan struct{})
	var exitErr error
	go func() {
		exitErr = cmd.Wait()
		close(exited)
	}()
	t.Cleanup(func() {
		if t.Failed() {
			t.Logf("axum oracle output:\n%s", log.String())
		}
		_ = syscall.Kill(-cmd.Process.Pid, syscall.SIGKILL)
		_ = cmd.Process.Kill()
		select {
		case <-exited:
		case <-time.After(3 * time.Second):
			t.Errorf("axum oracle did not exit after kill")
		}
	})
	addr := net.JoinHostPort("127.0.0.1", strconv.Itoa(port))
	deadline := time.Now().Add(30 * time.Second)
	for time.Now().Before(deadline) {
		select {
		case <-exited:
			t.Fatalf("axum oracle exited during startup: %v\n%s", exitErr, log.String())
		default:
		}
		conn, err := net.DialTimeout("tcp", addr, 150*time.Millisecond)
		if err == nil {
			conn.Close()
			return addr
		}
		time.Sleep(50 * time.Millisecond)
	}
	t.Fatalf("axum oracle did not listen on %s within 30s\n%s", addr, log.String())
	return ""
}

func parityClient(addr string, h2 bool) (*http.Client, func()) {
	if !h2 {
		transport := &http.Transport{MaxIdleConnsPerHost: 2}
		return &http.Client{Transport: transport, Timeout: 8 * time.Second}, transport.CloseIdleConnections
	}
	transport := &http2.Transport{
		AllowHTTP: true,
		DialTLSContext: func(ctx context.Context, network, _ string, _ *tls.Config) (net.Conn, error) {
			return (&net.Dialer{Timeout: 2 * time.Second}).DialContext(ctx, network, addr)
		},
	}
	return &http.Client{Transport: transport, Timeout: 8 * time.Second}, transport.CloseIdleConnections
}

func requestResult(client *http.Client, addr, method, path string, body []byte) (parityResult, error) {
	req, err := http.NewRequest(method, "http://"+addr+path, bytes.NewReader(body))
	if err != nil {
		return parityResult{}, err
	}
	if method == http.MethodPost {
		req.Header.Set("Content-Type", "application/octet-stream")
	}
	resp, err := client.Do(req)
	if err != nil {
		return parityResult{}, err
	}
	defer resp.Body.Close()
	got, err := io.ReadAll(io.LimitReader(resp.Body, 100_001))
	if err != nil {
		return parityResult{}, err
	}
	return parityResult{status: resp.StatusCode, proto: resp.ProtoMajor, body: got}, nil
}

func TestReferenceParityHTTP(t *testing.T) {
	woo := startWoo(t)
	reference := startOracle(t)
	for _, protocol := range []struct {
		name string
		h2   bool
	}{{"http1", false}, {"h2c", true}} {
		t.Run(protocol.name, func(t *testing.T) {
			wooClient, closeWoo := parityClient(woo, protocol.h2)
			refClient, closeRef := parityClient(reference, protocol.h2)
			t.Cleanup(closeWoo)
			t.Cleanup(closeRef)
			hegel.Test(t, func(ht *hegel.T) {
				ids := hegel.Draw(ht, hegel.Lists(hegel.Integers(0, 9999)).MinSize(1).MaxSize(8))
				body := hegel.Draw(ht, hegel.Binary(1, 4096))
				for i, id := range ids {
					path := fmt.Sprintf("/echo/%d-%d", i, id)
					checkParityRequest(ht, wooClient, refClient, woo, reference,
						http.MethodGet, path, nil, protocol.h2, []byte(path))
				}
				checkParityRequest(ht, wooClient, refClient, woo, reference,
					http.MethodPost, "/body", body, protocol.h2, body)
			}, hegel.WithTestCases(35))
		})
	}
}

func checkParityRequest(ht *hegel.T, wooClient, refClient *http.Client,
	woo, reference, method, path string, body []byte, h2 bool, expected []byte) {
	ht.Helper()
	left, err := requestResult(wooClient, woo, method, path, body)
	if err != nil {
		ht.Fatalf("Woo %s %s: %v", method, path, err)
	}
	right, err := requestResult(refClient, reference, method, path, body)
	if err != nil {
		ht.Fatalf("reference %s %s: %v", method, path, err)
	}
	wantProto := 1
	if h2 {
		wantProto = 2
	}
	if left.status != 200 || right.status != 200 || left.proto != wantProto || right.proto != wantProto ||
		!bytes.Equal(left.body, expected) || !bytes.Equal(right.body, expected) ||
		!bytes.Equal(left.body, right.body) {
		ht.Fatalf("%s %s: Woo=%+v reference=%+v wantProto=%d wantBody=%x",
			method, path, left, right, wantProto, expected)
	}
}

func websocketEcho(addr string, payload, mask []byte, key string) ([]byte, error) {
	conn, err := net.DialTimeout("tcp", addr, 2*time.Second)
	if err != nil {
		return nil, err
	}
	defer conn.Close()
	conn.SetDeadline(time.Now().Add(5 * time.Second))
	request := fmt.Sprintf("GET /ws HTTP/1.1\r\nHost: localhost\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: %s\r\nSec-WebSocket-Version: 13\r\n\r\n", key)
	if _, err := io.WriteString(conn, request); err != nil {
		return nil, err
	}
	reader := bufio.NewReader(conn)
	resp, err := http.ReadResponse(reader, nil)
	if err != nil {
		return nil, err
	}
	resp.Body.Close()
	accept := sha1.Sum([]byte(key + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"))
	wantAccept := base64.StdEncoding.EncodeToString(accept[:])
	if resp.StatusCode != 101 || resp.Header.Get("Sec-WebSocket-Accept") != wantAccept {
		return nil, fmt.Errorf("upgrade status=%d accept=%q want=%q", resp.StatusCode,
			resp.Header.Get("Sec-WebSocket-Accept"), wantAccept)
	}
	if _, err := conn.Write(maskedBinaryFrame(payload, mask)); err != nil {
		return nil, err
	}
	return readServerBinaryFrame(reader)
}

func TestReferenceParityWebSocket(t *testing.T) {
	woo := startWoo(t)
	reference := startOracle(t)
	hegel.Test(t, func(ht *hegel.T) {
		payload := hegel.Draw(ht, hegel.Binary(0, 1024))
		mask := hegel.Draw(ht, hegel.Binary(4, 4))
		keyBytes := hegel.Draw(ht, hegel.Binary(16, 16))
		key := base64.StdEncoding.EncodeToString(keyBytes)
		left, err := websocketEcho(woo, payload, mask, key)
		if err != nil {
			ht.Fatalf("Woo WebSocket: %v", err)
		}
		right, err := websocketEcho(reference, payload, mask, key)
		if err != nil {
			ht.Fatalf("reference WebSocket: %v", err)
		}
		if !bytes.Equal(left, payload) || !bytes.Equal(right, payload) || !bytes.Equal(left, right) {
			ht.Fatalf("WebSocket echo: Woo=%x reference=%x expected=%x", left, right, payload)
		}
	}, hegel.WithTestCases(40))
}
