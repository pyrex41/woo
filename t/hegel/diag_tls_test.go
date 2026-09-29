package hegeltest

import (
	"bytes"
	"context"
	"crypto/sha256"
	"crypto/tls"
	"crypto/x509"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	"os"
	"strconv"
	"strings"
	"testing"
	"time"
)

type diagReadResult struct {
	bytes int
	dur   time.Duration
	err   error
	sha   string
}

func diagSlowRead(t *testing.T, addr string, roots *x509.CertPool, body []byte, readBuffer int) diagReadResult {
	t.Helper()
	conn, err := net.DialTimeout("tcp", addr, 5*time.Second)
	if err != nil {
		t.Fatal(err)
	}
	tcp := conn.(*net.TCPConn)
	if err := tcp.SetReadBuffer(readBuffer); err != nil {
		t.Fatal(err)
	}
	defer conn.Close()
	tlsConn := tls.Client(tcp, tlsLiveTLSConfig(roots, "http/1.1"))
	if err := tlsConn.Handshake(); err != nil {
		t.Fatal(err)
	}
	if _, err := io.WriteString(tlsConn, "GET /large-static HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"); err != nil {
		t.Fatal(err)
	}
	start := time.Now()
	deadline := start.Add(30 * time.Second)
	buf := make([]byte, 4096)
	var response bytes.Buffer
	var readErr error
	for time.Now().Before(deadline) {
		readDeadline := time.Now().Add(5 * time.Second)
		if readDeadline.After(deadline) {
			readDeadline = deadline
		}
		if err := tlsConn.SetReadDeadline(readDeadline); err != nil {
			t.Fatal(err)
		}
		n, err := tlsConn.Read(buf)
		response.Write(buf[:n])
		if err == io.EOF {
			break
		}
		if err != nil {
			readErr = err
			break
		}
		time.Sleep(2 * time.Millisecond)
	}
	if readErr == nil && time.Now().After(deadline) {
		readErr = context.DeadlineExceeded
	}
	var bodyGot []byte
	if parts := bytes.SplitN(response.Bytes(), []byte("\r\n\r\n"), 2); len(parts) == 2 {
		bodyGot = parts[1]
	}
	return diagReadResult{bytes: len(response.Bytes()), dur: time.Since(start), err: readErr, sha: fmt.Sprintf("%x/%x", sha256.Sum256(bodyGot), sha256.Sum256(body))}
}

func startDiagReference(t *testing.T, certs tlsLiveCertificates, body []byte) *httptest.Server {
	t.Helper()
	cert, err := tls.LoadX509KeyPair(certs.certA, certs.keyA)
	if err != nil {
		t.Fatal(err)
	}
	s := httptest.NewUnstartedServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Length", strconv.Itoa(len(body)))
		w.WriteHeader(http.StatusOK)
		_, _ = w.Write(body)
	}))
	s.TLS = &tls.Config{Certificates: []tls.Certificate{cert}, MinVersion: tls.VersionTLS12, NextProtos: []string{"http/1.1"}}
	s.StartTLS()
	t.Cleanup(s.Close)
	return s
}

func TestDiagnosticTLSReferenceAndWoo(t *testing.T) {
	certs := makeTLSLiveCertificates(t)
	body, err := os.ReadFile(certs.body)
	if err != nil {
		t.Fatal(err)
	}
	roots := tlsLiveRoots(t, certs.root)
	ref := startDiagReference(t, certs, body)
	for _, size := range []int{1024, 16 * 1024} {
		refResult := diagSlowRead(t, strings.TrimPrefix(ref.URL, "https://"), roots, body, size)
		t.Logf("reference read_buffer=%d bytes=%d duration=%s err=%v sha=%s", size, refResult.bytes, refResult.dur, refResult.err, refResult.sha)
		if refResult.err != nil {
			t.Errorf("reference read_buffer=%d: %v", size, refResult.err)
		}
		if !strings.HasSuffix(refResult.sha, fmt.Sprintf("/%x", sha256.Sum256(body))) {
			t.Errorf("reference digest mismatch: %s", refResult.sha)
		}

		port := tlsLivePort(t)
		t.Setenv("WOO_HEGEL_PORT", port)
		configureTLSLive(t, certs)
		addr, err := startFixture(t, fixtureSpec{name: "Woo diagnostic", command: tlsLiveCommand(t), readyPath: "/.woo-test-ready", startupTTL: 60 * time.Second}, port)
		if err != nil {
			t.Fatal(err)
		}
		result := diagSlowRead(t, net.JoinHostPort("127.0.0.1", strconv.Itoa(mustPort(t, port)+1)), roots, body, size)
		t.Logf("woo read_buffer=%d bytes=%d duration=%s err=%v sha=%s", size, result.bytes, result.dur, result.err, result.sha)
		_ = addr
	}
}
