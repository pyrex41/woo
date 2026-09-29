package hegeltest

import (
	"bufio"
	"crypto/tls"
	"crypto/x509"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"strconv"
	"testing"
	"time"
)

// TestManagedHTTPSOversizeWire413 checks the wire contract without an HTTP
// client's concurrent request-body upload. The managed parser rejects the
// declared size from the request headers, so a client must be able to observe
// the 413 before sending the body.
func TestManagedHTTPSOversizeWire413(t *testing.T) {
	addr := managedFixture(t)
	certRoot := os.Getenv("WOO_COMPAT_CERT_ROOT")
	if certRoot == "" {
		certRoot = filepath.Join("..", "certs")
	}
	cert, err := os.ReadFile(filepath.Join(certRoot, "localCA.crt"))
	if err != nil {
		t.Fatal(err)
	}
	roots := x509.NewCertPool()
	if !roots.AppendCertsFromPEM(cert) {
		t.Fatal("invalid fixture CA")
	}
	host, portText, err := net.SplitHostPort(addr)
	if err != nil {
		t.Fatal(err)
	}
	port, err := strconv.Atoi(portText)
	if err != nil {
		t.Fatal(err)
	}
	secureAddr := net.JoinHostPort(host, strconv.Itoa(port+1))

	for attempt := 0; attempt < 5; attempt++ {
		conn, err := tls.DialWithDialer(&net.Dialer{Timeout: 5 * time.Second}, "tcp", secureAddr, &tls.Config{
			RootCAs:    roots,
			ServerName: "localhost",
			MinVersion: tls.VersionTLS12,
		})
		if err != nil {
			t.Fatalf("TLS dial attempt %d: %v", attempt, err)
		}
		_ = conn.SetDeadline(time.Now().Add(5 * time.Second))
		_, err = fmt.Fprintf(conn, "POST /echo HTTP/1.1\r\nHost: localhost\r\nContent-Length: 131073\r\nConnection: close\r\n\r\n")
		if err != nil {
			conn.Close()
			t.Fatalf("write headers attempt %d: %v", attempt, err)
		}
		response, err := http.ReadResponse(bufio.NewReader(conn), nil)
		if err != nil {
			conn.Close()
			t.Fatalf("read response attempt %d: %v", attempt, err)
		}
		body, readErr := io.ReadAll(io.LimitReader(response.Body, 4096))
		response.Body.Close()
		conn.Close()
		if readErr != nil {
			t.Fatalf("read body attempt %d: %v", attempt, readErr)
		}
		if response.StatusCode != http.StatusRequestEntityTooLarge {
			t.Fatalf("attempt %d: status=%d body=%q", attempt, response.StatusCode, body)
		}
		if string(body) != "413 Request Entity Too Large" {
			t.Fatalf("attempt %d: body=%q", attempt, body)
		}
		if response.ContentLength != int64(len(body)) {
			t.Fatalf("attempt %d: content-length=%d body-length=%d", attempt, response.ContentLength, len(body))
		}
	}
	assertManagedHTTPSHealthy(t, addr)
}

// TestManagedHTTPSOversizeConcurrentWire413 keeps the request upload in flight
// while reading the response. A client write can legitimately see a broken
// pipe after the server rejects the declared size; the response is still the
// observable contract this test records.
func TestManagedHTTPSOversizeConcurrentWire413(t *testing.T) {
	addr := managedFixture(t)
	certRoot := os.Getenv("WOO_COMPAT_CERT_ROOT")
	if certRoot == "" {
		certRoot = filepath.Join("..", "certs")
	}
	cert, err := os.ReadFile(filepath.Join(certRoot, "localCA.crt"))
	if err != nil {
		t.Fatal(err)
	}
	roots := x509.NewCertPool()
	if !roots.AppendCertsFromPEM(cert) {
		t.Fatal("invalid fixture CA")
	}
	host, portText, err := net.SplitHostPort(addr)
	if err != nil {
		t.Fatal(err)
	}
	port, err := strconv.Atoi(portText)
	if err != nil {
		t.Fatal(err)
	}
	secureAddr := net.JoinHostPort(host, strconv.Itoa(port+1))

	for attempt := 0; attempt < 5; attempt++ {
		conn, err := tls.DialWithDialer(&net.Dialer{Timeout: 5 * time.Second}, "tcp", secureAddr, &tls.Config{
			RootCAs:    roots,
			ServerName: "localhost",
			MinVersion: tls.VersionTLS12,
		})
		if err != nil {
			t.Fatalf("TLS dial attempt %d: %v", attempt, err)
		}
		_ = conn.SetDeadline(time.Now().Add(5 * time.Second))
		if _, err = fmt.Fprintf(conn, "POST /echo HTTP/1.1\r\nHost: localhost\r\nContent-Length: 131073\r\nConnection: close\r\n\r\n"); err != nil {
			conn.Close()
			t.Fatalf("write headers attempt %d: %v", attempt, err)
		}
		writeDone := make(chan error, 1)
		go func() {
			body := make([]byte, 131073)
			for offset := 0; offset < len(body); {
				end := offset + 1024
				if end > len(body) {
					end = len(body)
				}
				start := offset
				n, writeErr := conn.Write(body[start:end])
				if n > 0 {
					offset += n
				}
				if writeErr != nil {
					writeDone <- writeErr
					return
				}
				if n != end-start {
					writeDone <- io.ErrShortWrite
					return
				}
			}
			writeDone <- nil
		}()
		response, err := http.ReadResponse(bufio.NewReader(conn), nil)
		if err != nil {
			conn.Close()
			select {
			case <-writeDone:
			case <-time.After(5 * time.Second):
				t.Fatal("writer did not stop after response read failure")
			}
			t.Fatalf("read response attempt %d: %v", attempt, err)
		}
		body, readErr := io.ReadAll(io.LimitReader(response.Body, 4096))
		response.Body.Close()
		if readErr != nil {
			conn.Close()
			select {
			case <-writeDone:
			case <-time.After(5 * time.Second):
				t.Fatal("writer did not stop after response body read failure")
			}
			t.Fatalf("read body attempt %d: %v", attempt, readErr)
		}
		writeErr := <-writeDone
		conn.Close()
		t.Logf("concurrent attempt %d: status=%d write_error=%v", attempt, response.StatusCode, writeErr)
		if response.StatusCode != http.StatusRequestEntityTooLarge || string(body) != "413 Request Entity Too Large" {
			t.Fatalf("attempt %d: status=%d body=%q write_error=%v", attempt, response.StatusCode, body, writeErr)
		}
		if response.ContentLength != int64(len(body)) {
			t.Fatalf("attempt %d: content-length=%d body-length=%d write_error=%v", attempt, response.ContentLength, len(body), writeErr)
		}
	}
	assertManagedHTTPSHealthy(t, addr)
}

func assertManagedHTTPSHealthy(t *testing.T, addr string) {
	t.Helper()
	lane := managedClients(t, addr)["https"]
	response, body := managedRequest(t, lane.client, "GET", lane.base+"/", nil, nil)
	if response.StatusCode != http.StatusOK || string(body) != "こんにちは λ" {
		t.Fatalf("HTTPS service unhealthy after rejection: status=%d body=%q", response.StatusCode, body)
	}
}
