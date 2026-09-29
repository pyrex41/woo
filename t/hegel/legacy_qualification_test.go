package hegeltest

import (
	"bufio"
	"bytes"
	"crypto/sha256"
	"crypto/tls"
	"encoding/json"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"strconv"
	"strings"
	"sync"
	"syscall"
	"testing"
	"time"
)

var legacyPIDs sync.Map

func startLegacy(t *testing.T) (string, string) {
	t.Helper()
	lisp := os.Getenv("WOO_HEGEL_LISP")
	if lisp == "" {
		lisp = "sbcl"
	}
	command := []string{lisp, "--script", "t/hegel/legacy-server.lisp"}
	if lisp == "ros" {
		command = []string{"ros", "-e", `(load "t/hegel/legacy-server.lisp")`}
	}
	legacyPID := 0
	requested := os.Getenv("WOO_HEGEL_PORT")
	if requested == "" {
		listener, err := net.Listen("tcp", "127.0.0.1:0")
		if err != nil {
			t.Fatal(err)
		}
		candidate := listener.Addr().(*net.TCPAddr).Port
		secureListener, err := net.Listen("tcp", net.JoinHostPort("127.0.0.1", strconv.Itoa(candidate+1)))
		listener.Close()
		if err == nil {
			secureListener.Close()
			requested = strconv.Itoa(candidate)
		}
	}
	plain, err := startFixture(t, fixtureSpec{
		name: "Woo legacy", command: command, readyPath: "/.woo-test-ready", startupTTL: 120 * time.Second,
		started: func(pid int) { legacyPID = pid },
	}, requested)
	if err != nil {
		t.Fatal(err)
	}
	host, portText, _ := net.SplitHostPort(plain)
	port, _ := strconv.Atoi(portText)
	legacyPIDs.Store(plain, legacyPID)
	return plain, net.JoinHostPort(host, strconv.Itoa(port+1))
}

func legacyClients(t *testing.T, plain, secure string) (map[string]*http.Client, func()) {
	t.Helper()
	plainTransport := &http.Transport{DisableCompression: true}
	tlsTransport := &http.Transport{DisableCompression: true, TLSClientConfig: &tls.Config{InsecureSkipVerify: true}} // fixture certificate
	return map[string]*http.Client{
		"http":  {Transport: plainTransport, Timeout: 8 * time.Second},
		"https": {Transport: tlsTransport, Timeout: 8 * time.Second},
	}, func() { plainTransport.CloseIdleConnections(); tlsTransport.CloseIdleConnections() }
}

func legacyRequest(t *testing.T, client *http.Client, base, method, path string, body []byte) (*http.Response, []byte) {
	t.Helper()
	req, err := http.NewRequest(method, base+path, bytes.NewReader(body))
	if err != nil {
		t.Fatal(err)
	}
	if len(body) > 0 {
		req.Header.Set("Content-Type", "application/octet-stream")
	}
	resp, err := client.Do(req)
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()
	data, err := io.ReadAll(io.LimitReader(resp.Body, 2*1024*1024+1))
	if err != nil {
		t.Fatal(err)
	}
	if len(data) > 2*1024*1024 {
		t.Fatal("response exceeds qualification budget")
	}
	return resp, data
}

func stopLegacy(t *testing.T, plain string) {
	t.Helper()
	path := os.Getenv("WOO_LEGACY_STOP_RESULT")
	if path == "" {
		t.Fatal("WOO_LEGACY_STOP_RESULT is required")
	}
	pidValue, ok := legacyPIDs.Load(plain)
	if !ok {
		t.Fatalf("missing legacy fixture PID for %s", plain)
	}
	pid := pidValue.(int)
	_ = os.Remove(path)
	client := &http.Client{Timeout: 3 * time.Second}
	resp, err := client.Get("http://" + plain + "/stop")
	if err != nil {
		t.Fatal(err)
	}
	resp.Body.Close()
	if resp.StatusCode != http.StatusAccepted {
		t.Fatalf("stop status %d", resp.StatusCode)
	}
	deadline := time.Now().Add(8 * time.Second)
	for time.Now().Before(deadline) {
		if data, err := os.ReadFile(path); err == nil && strings.TrimSpace(string(data)) == "PASS" {
			for _, address := range []string{plain, secureAddress(plain)} {
				for time.Now().Before(deadline) {
					conn, err := net.DialTimeout("tcp", address, 200*time.Millisecond)
					if err != nil {
						break
					}
					conn.Close()
					time.Sleep(50 * time.Millisecond)
				}
				if conn, err := net.DialTimeout("tcp", address, 200*time.Millisecond); err == nil {
					conn.Close()
					t.Fatalf("listener %s still accepts connections after stop", address)
				}
			}
			for time.Now().Before(deadline) {
				if err := syscall.Kill(pid, 0); err != nil {
					legacyPIDs.Delete(plain)
					return
				}
				time.Sleep(50 * time.Millisecond)
			}
			t.Fatalf("legacy fixture process %d remains alive after stop", pid)
		}
		time.Sleep(50 * time.Millisecond)
	}
	t.Fatal("legacy stop did not report PASS")
}

func secureAddress(plain string) string {
	host, portText, _ := net.SplitHostPort(plain)
	port, _ := strconv.Atoi(portText)
	return net.JoinHostPort(host, strconv.Itoa(port+1))
}

func assertSpoolEmpty(t *testing.T) {
	t.Helper()
	entries, err := os.ReadDir(os.Getenv("WOO_LEGACY_SPOOL_ROOT"))
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) != 0 {
		t.Fatalf("smart-buffer spool files remain: %v", entries)
	}
}

type delayedReader struct{ reader io.Reader }

func (r delayedReader) Read(p []byte) (int, error) {
	n, err := r.reader.Read(p)
	if n > 0 {
		time.Sleep(4 * time.Millisecond)
	}
	return n, err
}

func TestLegacyQualification(t *testing.T) {
	if os.Getenv("WOO_RUN_LEGACY_QUALIFICATION") != "1" {
		t.Skip("legacy qualification is opt-in; run t/qualification/check.py")
	}
	plain, secure := startLegacy(t)
	clients, closeClients := legacyClients(t, plain, secure)
	defer closeClients()
	for name, client := range clients {
		base := "http://" + plain
		if name == "https" {
			base = "https://" + secure
		}
		t.Run(name+"/statuses-static-upload", func(t *testing.T) {
			for _, code := range []int{200, 204, 404, 425, 428, 429, 431, 500, 511} {
				resp, body := legacyRequest(t, client, base, "GET", fmt.Sprintf("/status/%d", code), nil)
				if resp.StatusCode != code {
					t.Fatalf("status %d returned %d", code, resp.StatusCode)
				}
				if code != 204 && string(body) != strconv.Itoa(code) {
					t.Fatalf("status body %q", body)
				}
			}
			resp, body := legacyRequest(t, client, base, "GET", "/static/fixture.txt", nil)
			want, err := os.ReadFile(os.Getenv("WOO_LEGACY_STATIC_FILE"))
			if err != nil {
				t.Fatal(err)
			}
			if resp.StatusCode != http.StatusOK || !bytes.Equal(body, want) {
				t.Fatal("static response mismatch")
			}
			payload := bytes.Repeat([]byte("woo"), 4096)
			resp, body = legacyRequest(t, client, base, "POST", "/upload", payload)
			if resp.StatusCode != http.StatusOK || !bytes.Equal(body, payload) {
				t.Fatalf("upload response mismatch: status=%d got=%d want=%d", resp.StatusCode, len(body), len(payload))
			}
			assertSpoolEmpty(t)
		})
	}

	t.Run("slow-reader", func(t *testing.T) {
		for _, target := range []struct {
			address string
			secure  bool
		}{{plain, false}, {secure, true}} {
			conn, err := net.DialTimeout("tcp", target.address, 2*time.Second)
			if err != nil {
				t.Fatal(err)
			}
			defer conn.Close()
			if target.secure {
				conn = tls.Client(conn, &tls.Config{InsecureSkipVerify: true})
			}
			_ = conn.SetDeadline(time.Now().Add(30 * time.Second))
			if tcp, ok := conn.(*net.TCPConn); ok {
				_ = tcp.SetReadBuffer(1024)
			}
			if _, err := io.WriteString(conn, "GET /stream HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"); err != nil {
				t.Fatal(err)
			}
			reader := bufio.NewReader(conn)
			response, err := http.ReadResponse(reader, nil)
			if err != nil {
				t.Fatal(err)
			}
			if response.StatusCode != http.StatusOK {
				t.Fatalf("stream status %d", response.StatusCode)
			}
			hash := sha256.New()
			seen, err := io.CopyBuffer(hash, delayedReader{response.Body}, make([]byte, 1024))
			if err != nil {
				t.Fatal(err)
			}
			response.Body.Close()
			want := sha256.Sum256(bytes.Repeat([]byte{'A'}, 128*1024))
			if seen != 128*1024 || !bytes.Equal(hash.Sum(nil), want[:]) {
				t.Fatalf("slow stream mismatch: bytes=%d", seen)
			}
		}
	})

	t.Run("oversized-and-disconnect", func(t *testing.T) {
		for _, target := range []struct {
			base, address string
			tls           bool
		}{{"http://" + plain, plain, false}, {"https://" + secure, secure, true}} {
			conn, err := net.DialTimeout("tcp", target.address, 2*time.Second)
			if err != nil {
				t.Fatal(err)
			}
			if target.tls {
				conn = tls.Client(conn, &tls.Config{InsecureSkipVerify: true})
			}
			_ = conn.SetDeadline(time.Now().Add(8 * time.Second))
			_, _ = io.WriteString(conn, "POST /upload HTTP/1.1\r\nHost: localhost\r\nContent-Length: 65537\r\nConnection: close\r\n\r\n")
			headers := make([]byte, 4096)
			n, _ := conn.Read(headers)
			_ = conn.Close()
			time.Sleep(250 * time.Millisecond)
			assertSpoolEmpty(t)
			if n < 12 || !bytes.Contains(headers[:n], []byte(" 413 ")) {
				t.Fatalf("oversize response was not 413: %q", headers[:n])
			}
		}
		{
			conn, err := net.DialTimeout("tcp", plain, 2*time.Second)
			if err != nil {
				t.Fatal(err)
			}
			_, _ = io.WriteString(conn, "POST /upload HTTP/1.1\r\nHost: localhost\r\nContent-Length: 65536\r\nConnection: close\r\n\r\npartial")
			_ = conn.Close()
		}
	})

	duration := 1800 * time.Second
	if raw := os.Getenv("WOO_LEGACY_SOAK_SECONDS"); raw != "" {
		seconds, err := strconv.Atoi(raw)
		if err != nil || seconds < 1 || seconds > 1800 {
			t.Fatal("invalid WOO_LEGACY_SOAK_SECONDS")
		}
		duration = time.Duration(seconds) * time.Second
	}
	deadline := time.Now().Add(duration)
	cycles := 0
	started := time.Now()
	fmt.Fprintln(os.Stdout, "LEGACY_PHASE soak-start")
	for time.Now().Before(deadline) {
		for name, client := range clients {
			base := "http://" + plain
			if name == "https" {
				base = "https://" + secure
			}
			path := "/slow"
			if cycles%4 == 1 {
				path = "/static/fixture.txt"
			}
			if cycles%4 == 2 {
				path = "/status/429"
			}
			if cycles%4 == 3 {
				path = "/upload"
			}
			payload := []byte(nil)
			if path == "/upload" {
				payload = bytes.Repeat([]byte("cycle"), 64)
			}
			method := "GET"
			if path == "/upload" {
				method = "POST"
			}
			resp, body := legacyRequest(t, client, base, method, path, payload)
			wantStatus, wantBody := 200, "slow"
			if path == "/static/fixture.txt" {
				wantBody = "woo legacy qualification fixture\n"
			}
			if path == "/status/429" {
				wantStatus, wantBody = 429, "429"
			}
			if path == "/upload" {
				wantBody = string(payload)
			}
			if resp.StatusCode != wantStatus || string(body) != wantBody {
				t.Fatalf("cycle %s response mismatch", path)
			}
		}
		cycles++
	}
	soakElapsed := time.Since(started).Seconds()
	fmt.Fprintln(os.Stdout, "LEGACY_PHASE soak-end")
	assertSpoolEmpty(t)
	stopLegacy(t, plain)
	for i := 0; i < 30; i++ {
		cyclePlain, _ := startLegacy(t)
		client := &http.Client{Timeout: 3 * time.Second}
		resp, err := client.Get("http://" + cyclePlain + "/.woo-test-ready")
		if err != nil {
			t.Fatal(err)
		}
		resp.Body.Close()
		stopLegacy(t, cyclePlain)
	}
	resultPath := os.Getenv("WOO_LEGACY_RESULT")
	if resultPath != "" {
		data, _ := json.Marshal(map[string]any{"cycles": cycles, "lifecycle_cycles": 30, "duration_seconds": time.Since(started).Seconds(), "soak_elapsed_seconds": soakElapsed, "spool_cleanup": "PASS"})
		if err := os.WriteFile(resultPath, append(data, '\n'), 0600); err != nil {
			t.Fatal(err)
		}
	}
}
