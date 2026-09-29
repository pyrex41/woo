package hegeltest

import (
	"bufio"
	"bytes"
	"context"
	"crypto/rand"
	"crypto/rsa"
	"crypto/sha256"
	"crypto/tls"
	"crypto/x509"
	"crypto/x509/pkix"
	"encoding/pem"
	"errors"
	"fmt"
	"io"
	"math/big"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"
)

type tlsLiveCertificates struct {
	dir, root, certA, certALeaf, keyA, certB, certBLeaf, keyB, badKey, body string
}

func makeTLSLiveCertificates(t *testing.T) tlsLiveCertificates {
	t.Helper()
	dir := t.TempDir()
	serial := int64(1)
	newKey := func() *rsa.PrivateKey {
		key, err := rsa.GenerateKey(rand.Reader, 2048)
		if err != nil {
			t.Fatal(err)
		}
		return key
	}
	writeKey := func(name string, key *rsa.PrivateKey) string {
		path := filepath.Join(dir, name)
		f, err := os.Create(path)
		if err != nil {
			t.Fatal(err)
		}
		err = pem.Encode(f, &pem.Block{Type: "RSA PRIVATE KEY", Bytes: x509.MarshalPKCS1PrivateKey(key)})
		if closeErr := f.Close(); err == nil {
			err = closeErr
		}
		if err != nil {
			t.Fatal(err)
		}
		return path
	}
	rootKey := newKey()
	now := time.Now()
	root := &x509.Certificate{SerialNumber: big.NewInt(serial), Subject: pkix.Name{CommonName: "Woo TLS Live Root"},
		NotBefore: now.Add(-time.Minute), NotAfter: now.Add(time.Hour), IsCA: true, BasicConstraintsValid: true,
		KeyUsage: x509.KeyUsageCertSign | x509.KeyUsageCRLSign | x509.KeyUsageDigitalSignature}
	serial++
	rootDER, err := x509.CreateCertificate(rand.Reader, root, root, &rootKey.PublicKey, rootKey)
	if err != nil {
		t.Fatal(err)
	}
	rootPath := filepath.Join(dir, "root.crt")
	writePEM(t, rootPath, "CERTIFICATE", rootDER)
	intermediateKey := newKey()
	intermediate := &x509.Certificate{SerialNumber: big.NewInt(serial), Subject: pkix.Name{CommonName: "Woo TLS Live Intermediate"},
		NotBefore: now.Add(-time.Minute), NotAfter: now.Add(time.Hour), IsCA: true, BasicConstraintsValid: true,
		KeyUsage: x509.KeyUsageCertSign | x509.KeyUsageDigitalSignature}
	serial++
	intermediateDER, err := x509.CreateCertificate(rand.Reader, intermediate, root, &intermediateKey.PublicKey, rootKey)
	if err != nil {
		t.Fatal(err)
	}
	intermediatePath := filepath.Join(dir, "intermediate.crt")
	writePEM(t, intermediatePath, "CERTIFICATE", intermediateDER)
	makeLeaf := func(name string) (string, string) {
		key := newKey()
		cert := &x509.Certificate{SerialNumber: big.NewInt(serial), Subject: pkix.Name{CommonName: name},
			DNSNames: []string{"localhost"}, NotBefore: now.Add(-time.Minute), NotAfter: now.Add(time.Hour),
			KeyUsage:    x509.KeyUsageDigitalSignature | x509.KeyUsageKeyEncipherment,
			ExtKeyUsage: []x509.ExtKeyUsage{x509.ExtKeyUsageServerAuth}}
		serial++
		der, err := x509.CreateCertificate(rand.Reader, cert, intermediate, &key.PublicKey, intermediateKey)
		if err != nil {
			t.Fatal(err)
		}
		certPath := filepath.Join(dir, name+".crt")
		writePEM(t, certPath, "CERTIFICATE", der)
		// Woo expects a complete chain in the certificate file.
		f, err := os.OpenFile(certPath, os.O_APPEND|os.O_WRONLY, 0600)
		if err != nil {
			t.Fatal(err)
		}
		if err := pem.Encode(f, &pem.Block{Type: "CERTIFICATE", Bytes: intermediateDER}); err != nil {
			t.Fatal(err)
		}
		f.Close()
		return certPath, writeKey(name+".key", key)
	}
	certA, keyA := makeLeaf("leaf-a")
	certB, keyB := makeLeaf("leaf-b")
	leafOnly := func(chainPath, name string) string {
		data, err := os.ReadFile(chainPath)
		if err != nil {
			t.Fatal(err)
		}
		block, _ := pem.Decode(data)
		if block == nil {
			t.Fatal("missing leaf PEM")
		}
		path := filepath.Join(dir, name)
		writePEM(t, path, "CERTIFICATE", block.Bytes)
		return path
	}
	certALeaf := leafOnly(certA, "leaf-a-only.crt")
	certBLeaf := leafOnly(certB, "leaf-b-only.crt")
	badKey := writeKey("bad.key", newKey())
	body := filepath.Join(dir, "body.bin")
	f, err := os.Create(body)
	if err != nil {
		t.Fatal(err)
	}
	chunk := make([]byte, 32*1024)
	for i := range chunk {
		chunk[i] = byte(i % 251)
	}
	for written := 0; written < 2*1024*1024; written += len(chunk) {
		if _, err := f.Write(chunk); err != nil {
			t.Fatal(err)
		}
	}
	if err := f.Close(); err != nil {
		t.Fatal(err)
	}
	return tlsLiveCertificates{dir: dir, root: rootPath, certA: certA, certALeaf: certALeaf, keyA: keyA, certB: certB, certBLeaf: certBLeaf, keyB: keyB, badKey: badKey, body: body}
}

func writePEM(t *testing.T, path, kind string, der []byte) {
	t.Helper()
	if err := os.WriteFile(path, pem.EncodeToMemory(&pem.Block{Type: kind, Bytes: der}), 0600); err != nil {
		t.Fatal(err)
	}
}

func configureTLSLive(t *testing.T, certs tlsLiveCertificates) {
	t.Helper()
	t.Setenv("WOO_TLS_CERT_A", certs.certA)
	t.Setenv("WOO_TLS_KEY_A", certs.keyA)
	t.Setenv("WOO_TLS_CERT_B", certs.certB)
	t.Setenv("WOO_TLS_KEY_B", certs.keyB)
	t.Setenv("WOO_TLS_STATIC_FILE", certs.body)
}

func tlsLiveCommand(t *testing.T) []string {
	t.Helper()
	lisp := os.Getenv("WOO_HEGEL_LISP")
	if lisp == "" {
		lisp = "sbcl"
	}
	if lisp == "ros" {
		return []string{"ros", "-e", `(load "t/hegel/tls-live-server.lisp")`}
	}
	return []string{lisp, "--script", "t/hegel/tls-live-server.lisp"}
}

func tlsLivePort(t *testing.T) string {
	t.Helper()
	l, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	port := l.Addr().(*net.TCPAddr).Port
	if err := l.Close(); err != nil {
		t.Fatal(err)
	}
	return strconv.Itoa(port)
}

func tlsLiveRoots(t *testing.T, certPath string) *x509.CertPool {
	t.Helper()
	pemBytes, err := os.ReadFile(certPath)
	if err != nil {
		t.Fatal(err)
	}
	roots := x509.NewCertPool()
	if !roots.AppendCertsFromPEM(pemBytes) {
		t.Fatal("root certificate did not parse")
	}
	return roots
}

func tlsLiveTLSConfig(roots *x509.CertPool, protocols ...string) *tls.Config {
	return &tls.Config{RootCAs: roots, MinVersion: tls.VersionTLS12, ServerName: "localhost", NextProtos: protocols}
}

func tlsLiveDial(t *testing.T, addr string, roots *x509.CertPool, protocols ...string) *tls.Conn {
	t.Helper()
	conn, err := tls.DialWithDialer(&net.Dialer{Timeout: 5 * time.Second}, "tcp", addr, tlsLiveTLSConfig(roots, protocols...))
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { conn.Close() })
	return conn
}

func TestTLSLiveChainALPNAndStaticBackpressure(t *testing.T) {
	certs := makeTLSLiveCertificates(t)
	configureTLSLive(t, certs)
	basePort := tlsLivePort(t)
	t.Setenv("WOO_HEGEL_PORT", basePort)
	for _, name := range []string{"first-start", "restart"} {
		t.Run(name, func(t *testing.T) {
			finalCounters := filepath.Join(t.TempDir(), "final-counters.txt")
			t.Setenv("WOO_TLS_FINAL_COUNTERS", finalCounters)
			addr, err := startFixture(t, fixtureSpec{name: "Woo live TLS", command: tlsLiveCommand(t), readyPath: "/.woo-test-ready", startupTTL: 60 * time.Second}, basePort)
			if err != nil {
				t.Fatal(err)
			}
			_ = addr
			port, _ := strconv.Atoi(basePort)
			roots := tlsLiveRoots(t, certs.root)
			a := tlsLiveDial(t, net.JoinHostPort("127.0.0.1", strconv.Itoa(port+1)), roots, "h2", "http/1.1")
			if got := a.ConnectionState().NegotiatedProtocol; got != "h2" {
				t.Fatalf("listener A ALPN=%q, want h2", got)
			}
			if got := a.ConnectionState().PeerCertificates[0].Subject.CommonName; got != "leaf-a" {
				t.Fatalf("listener A certificate=%q", got)
			}
			b := tlsLiveDial(t, net.JoinHostPort("127.0.0.1", strconv.Itoa(port+2)), roots, "h2", "http/1.1")
			if got := b.ConnectionState().NegotiatedProtocol; got != "http/1.1" {
				t.Fatalf("listener B ALPN=%q, want http/1.1", got)
			}
			if got := b.ConnectionState().PeerCertificates[0].Subject.CommonName; got != "leaf-b" {
				t.Fatalf("listener B certificate=%q", got)
			}
			baseFD, baseCtx := tlsLiveReadCounters(t, port, roots)
			tlsLiveSlowStatic(t, port+1, roots, certs.body)
			fd, ctx := tlsLiveReadCounters(t, port, roots)
			if ctx != baseCtx {
				t.Fatalf("ALPN context registry leaked: baseline=%d after=%d", baseCtx, ctx)
			}
			if baseFD >= 0 && fd > baseFD+2 {
				t.Fatalf("file descriptors did not return near baseline: baseline=%d after=%d", baseFD, fd)
			}
			tlsLiveStopAndCheckCleanup(t, port, finalCounters)
		})
	}
	t.Run("missing-intermediate-fails-with-root-only", func(t *testing.T) {
		t.Setenv("WOO_TLS_CERT_A", certs.certALeaf)
		t.Setenv("WOO_TLS_CERT_B", certs.certBLeaf)
		_, err := startFixture(t, fixtureSpec{name: "Woo missing intermediate", command: tlsLiveCommand(t), readyPath: "/.woo-test-ready", startupTTL: 60 * time.Second}, basePort)
		if err != nil {
			t.Fatal(err)
		}
		roots := tlsLiveRoots(t, certs.root)
		_, err = tls.DialWithDialer(&net.Dialer{Timeout: 5 * time.Second}, "tcp", net.JoinHostPort("127.0.0.1", strconv.Itoa(mustPort(t, basePort)+1)), tlsLiveTLSConfig(roots, "http/1.1"))
		if err == nil {
			t.Fatal("root-only client accepted server without intermediate")
		}
		var unknownAuthority x509.UnknownAuthorityError
		if !errors.As(err, &unknownAuthority) {
			t.Fatalf("missing-intermediate error=%T %v, want x509.UnknownAuthorityError", err, err)
		}
	})
}

func mustPort(t *testing.T, value string) int {
	t.Helper()
	port, err := strconv.Atoi(value)
	if err != nil {
		t.Fatal(err)
	}
	return port
}

func tlsLiveSlowStatic(t *testing.T, port int, roots *x509.CertPool, bodyPath string) {
	t.Helper()
	conn, err := net.DialTimeout("tcp", net.JoinHostPort("127.0.0.1", strconv.Itoa(port)), 5*time.Second)
	if err != nil {
		t.Fatal(err)
	}
	defer conn.Close()
	tcp := conn.(*net.TCPConn)
	// A 1 KiB receive window stalls both Woo and Go's TLS reference on
	// Linux. Keep backpressure without making TCP window probes the gate.
	if err := tcp.SetReadBuffer(16 * 1024); err != nil {
		t.Fatal(err)
	}
	tlsConn := tls.Client(tcp, tlsLiveTLSConfig(roots, "http/1.1"))
	handshakeContext, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	if err := tlsConn.HandshakeContext(handshakeContext); err != nil {
		t.Fatal(err)
	}
	if err := tlsConn.SetWriteDeadline(time.Now().Add(5 * time.Second)); err != nil {
		t.Fatal(err)
	}
	if _, err := io.WriteString(tlsConn, "GET /large-static HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"); err != nil {
		t.Fatal(err)
	}
	// Read a little, pause, and then throttle each read to force SSL_write
	// retries while retaining an exact end-to-end digest.
	buf := make([]byte, 4096)
	var response bytes.Buffer
	deadline := time.Now().Add(30 * time.Second)
	cleanEOF := false
	for time.Now().Before(deadline) {
		readDeadline := time.Now().Add(5 * time.Second)
		if readDeadline.After(deadline) {
			readDeadline = deadline
		}
		if err := tlsConn.SetReadDeadline(readDeadline); err != nil {
			t.Fatal(err)
		}
		n, readErr := tlsConn.Read(buf)
		response.Write(buf[:n])
		if readErr == io.EOF {
			cleanEOF = true
			break
		}
		if readErr != nil {
			t.Fatal(readErr)
		}
		time.Sleep(2 * time.Millisecond)
	}
	if !cleanEOF {
		t.Fatal("slow TLS static response did not reach clean EOF within 30 seconds")
	}
	reader := bufio.NewReader(bytes.NewReader(response.Bytes()))
	parsed, err := http.ReadResponse(reader, &http.Request{Method: http.MethodGet})
	if err != nil {
		t.Fatalf("invalid static response: %v", err)
	}
	defer parsed.Body.Close()
	want, err := os.ReadFile(bodyPath)
	if err != nil {
		t.Fatal(err)
	}
	if parsed.StatusCode != http.StatusOK || parsed.ContentLength != int64(len(want)) {
		t.Fatalf("static response status=%d content_length=%d, want 200/%d", parsed.StatusCode, parsed.ContentLength, len(want))
	}
	got, err := io.ReadAll(parsed.Body)
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(got, want) {
		t.Fatalf("static body mismatch: got=%d want=%d sha=%x/%x", len(got), len(want), sha256.Sum256(got), sha256.Sum256(want))
	}
	if extra, err := io.ReadAll(reader); err != nil || len(extra) != 0 {
		t.Fatalf("unexpected bytes after static body: bytes=%d err=%v", len(extra), err)
	}
	_ = tlsConn.Close()

	// A second connection is abandoned mid-body; the fixture must reclaim it.
	dead, err := tls.DialWithDialer(&net.Dialer{Timeout: 5 * time.Second}, "tcp", net.JoinHostPort("127.0.0.1", strconv.Itoa(port)), tlsLiveTLSConfig(roots, "http/1.1"))
	if err != nil {
		t.Fatal(err)
	}
	_, _ = io.WriteString(dead, "GET /large-static HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	dead.Close()
}

func tlsLiveReadCounters(t *testing.T, port int, roots *x509.CertPool) (int, int) {
	t.Helper()
	client := &http.Client{Timeout: 5 * time.Second, Transport: &http.Transport{TLSClientConfig: tlsLiveTLSConfig(roots, "http/1.1"), ForceAttemptHTTP2: false}}
	resp, err := client.Get("https://" + net.JoinHostPort("127.0.0.1", strconv.Itoa(port+1)) + "/counters")
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()
	defer client.Transport.(*http.Transport).CloseIdleConnections()
	data, err := io.ReadAll(resp.Body)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.HasPrefix(string(data), "fd=") || !strings.Contains(string(data), " ctx=") {
		t.Fatalf("unexpected cleanup counters %q", data)
	}
	var fd, ctx int
	if _, err := fmt.Sscanf(string(data), "fd=%d ctx=%d", &fd, &ctx); err != nil {
		t.Fatal(err)
	}
	return fd, ctx
}

func TestTLSLiveRejectsWrongKey(t *testing.T) {
	certs := makeTLSLiveCertificates(t)
	configureTLSLive(t, certs)
	t.Setenv("WOO_TLS_BAD_KEY", certs.badKey)
	port := tlsLivePort(t)
	t.Setenv("WOO_HEGEL_PORT", port)
	t.Setenv("WOO_HEGEL_READY_NONCE", "wrong-key-test")
	_, err := startFixture(t, fixtureSpec{name: "Woo wrong-key TLS", command: tlsLiveCommand(t), readyPath: "/.woo-test-ready", startupTTL: 60 * time.Second}, port)
	if err == nil {
		t.Fatal("wrong-key fixture unexpectedly became ready")
	}
	message := strings.ToLower(err.Error())
	if !strings.Contains(message, "tls certificate and private key do not match") {
		t.Fatalf("wrong-key startup error lacked key-mismatch evidence: %v", err)
	}
}

func tlsLiveStopAndCheckCleanup(t *testing.T, port int, countersPath string) {
	t.Helper()
	conn, err := net.DialTimeout("tcp", net.JoinHostPort("127.0.0.1", strconv.Itoa(port)), 5*time.Second)
	if err != nil {
		t.Fatal(err)
	}
	// The endpoint starts graceful shutdown, so the response itself may be
	// interrupted as the plain listener closes. The request write is the
	// synchronization point; the final counter file proves the stop completed.
	_ = conn.SetWriteDeadline(time.Now().Add(2 * time.Second))
	if _, err := io.WriteString(conn, "GET /stop HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"); err != nil {
		t.Fatal(err)
	}
	_ = conn.Close()
	deadline := time.Now().Add(10 * time.Second)
	for time.Now().Before(deadline) {
		data, readErr := os.ReadFile(countersPath)
		if readErr == nil {
			var fd, ctx int
			if _, scanErr := fmt.Sscanf(string(data), "fd=%d ctx=%d", &fd, &ctx); scanErr != nil {
				// The fixture creates the receipt before writing its final
				// line. Retry while the graceful shutdown receipt is settling.
				time.Sleep(25 * time.Millisecond)
				continue
			}
			if fd <= 0 {
				t.Fatalf("final file descriptor count is not positive: %d", fd)
			}
			if ctx != 0 {
				t.Fatalf("final ALPN context registry is not empty: %d", ctx)
			}
			return
		}
		time.Sleep(50 * time.Millisecond)
	}
	t.Fatalf("fixture did not publish graceful cleanup counters at %s", countersPath)
}
