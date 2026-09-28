package hegeltest

import (
	"bytes"
	"crypto/tls"
	"encoding/json"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"strconv"
	"strings"
	"testing"
	"time"
)

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
	}, requested)
	if err != nil {
		t.Fatal(err)
	}
	host, portText, _ := net.SplitHostPort(plain)
	port, _ := strconv.Atoi(portText)
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

func TestLegacyQualification(t *testing.T) {
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
				t.Fatal("upload response mismatch")
			}
		})
	}

	t.Run("slow-reader", func(t *testing.T) {
		conn, err := net.DialTimeout("tcp", plain, 2*time.Second)
		if err != nil {
			t.Fatal(err)
		}
		defer conn.Close()
		_ = conn.SetDeadline(time.Now().Add(8 * time.Second))
		if _, err := io.WriteString(conn, "GET /slow HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"); err != nil {
			t.Fatal(err)
		}
		buf := make([]byte, 1)
		seen := 0
		for {
			n, err := conn.Read(buf)
			seen += n
			if err != nil {
				if err != io.EOF {
					t.Fatal(err)
				}
				break
			}
			time.Sleep(2 * time.Millisecond)
		}
		if seen == 0 {
			t.Fatal("slow reader received no response")
		}
	})

	t.Run("oversized-and-disconnect", func(t *testing.T) {
		for _, target := range []string{"http://" + plain, "https://" + secure} {
			client := clients["http"]
			if strings.HasPrefix(target, "https") {
				client = clients["https"]
			}
			payload := bytes.Repeat([]byte("x"), 65537)
			req, err := http.NewRequest("POST", target+"/upload", bytes.NewReader(payload))
			if err != nil {
				t.Fatal(err)
			}
			resp, err := client.Do(req)
			if err == nil {
				resp.Body.Close()
				if resp.StatusCode != http.StatusRequestEntityTooLarge {
					t.Fatalf("oversize status %d", resp.StatusCode)
				}
			}
		}
		conn, err := net.DialTimeout("tcp", plain, 2*time.Second)
		if err != nil {
			t.Fatal(err)
		}
		_, _ = io.WriteString(conn, "POST /upload HTTP/1.1\r\nHost: localhost\r\nContent-Length: 65536\r\n\r\npartial")
		_ = conn.Close()
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
	for time.Now().Before(deadline) {
		for name, client := range clients {
			base := "http://" + plain
			if name == "https" {
				base = "https://" + secure
			}
			resp, body := legacyRequest(t, client, base, "GET", "/slow", nil)
			if resp.StatusCode != 200 || string(body) != "slow" {
				t.Fatal("cycle response mismatch")
			}
		}
		cycles++
	}
	resultPath := os.Getenv("WOO_LEGACY_RESULT")
	if resultPath != "" {
		data, _ := json.Marshal(map[string]any{"cycles": cycles, "duration_seconds": duration.Seconds()})
		if err := os.WriteFile(resultPath, append(data, '\n'), 0600); err != nil {
			t.Fatal(err)
		}
	}
}
