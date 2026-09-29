package hegeltest

import (
	"bufio"
	"bytes"
	"compress/gzip"
	"context"
	"crypto/tls"
	"crypto/x509"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/http/cookiejar"
	"net/url"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"testing"
	"time"

	"golang.org/x/net/http2"
	"golang.org/x/net/websocket"
)

var managedProcesses sync.Map

func managedFixture(t *testing.T) string {
	t.Helper()
	lisp := os.Getenv("WOO_HEGEL_LISP")
	if lisp == "" {
		lisp = "sbcl"
	}
	// Both listeners need free ports. Bound retries before starting the child.
	port := 0
	for attempt := 0; attempt < 10; attempt++ {
		plain, err := net.Listen("tcp", "127.0.0.1:0")
		if err != nil {
			t.Fatal(err)
		}
		candidate := plain.Addr().(*net.TCPAddr).Port
		secure, err := net.Listen("tcp", net.JoinHostPort("127.0.0.1", strconv.Itoa(candidate+1)))
		plain.Close()
		if err == nil {
			secure.Close()
			port = candidate
			break
		}
	}
	if port == 0 {
		t.Fatal("cannot reserve managed fixture port pair")
	}
	pid := 0
	addr, err := startFixture(t, fixtureSpec{name: "Woo managed", started: func(value int) { pid = value }, command: []string{lisp, "--script", "t/compat/server.lisp"}, readyPath: "/.woo-test-ready", startupTTL: 120 * time.Second}, strconv.Itoa(port))
	if err != nil {
		t.Fatal(err)
	}
	managedProcesses.Store(addr, pid)
	t.Cleanup(func() { managedProcesses.Delete(addr) })
	return addr
}
func managedClients(t *testing.T, addr string) map[string]struct {
	client *http.Client
	base   string
} {
	t.Helper()
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
	config := &tls.Config{RootCAs: roots, MinVersion: tls.VersionTLS12}
	host, p, _ := net.SplitHostPort(addr)
	port, _ := strconv.Atoi(p)
	secure := net.JoinHostPort(host, strconv.Itoa(port+1))
	result := map[string]struct {
		client *http.Client
		base   string
	}{}
	for name, transport := range map[string]http.RoundTripper{
		"http1": &http.Transport{DisableCompression: true, ExpectContinueTimeout: 5 * time.Second},
		"https": &http.Transport{TLSClientConfig: config.Clone(), DisableCompression: true, ForceAttemptHTTP2: false, ExpectContinueTimeout: 5 * time.Second},
		"h2c": &http2.Transport{DisableCompression: true, AllowHTTP: true, DialTLSContext: func(ctx context.Context, network, addr string, _ *tls.Config) (net.Conn, error) {
			return (&net.Dialer{Timeout: 5 * time.Second}).DialContext(ctx, network, addr)
		}},
		"h2tls": &http2.Transport{DisableCompression: true, TLSClientConfig: config.Clone()},
	} {
		base := "http://" + addr
		if name == "https" || name == "h2tls" {
			base = "https://" + secure
		}
		jar, _ := cookiejar.New(nil)
		result[name] = struct {
			client *http.Client
			base   string
		}{&http.Client{Transport: transport, Jar: jar, Timeout: 10 * time.Second}, base}
		if closer, ok := transport.(interface{ CloseIdleConnections() }); ok {
			t.Cleanup(closer.CloseIdleConnections)
		}
	}
	return result
}
func managedRequest(t *testing.T, c *http.Client, method, location string, body io.Reader, headers map[string]string) (*http.Response, []byte) {
	t.Helper()
	r, e := http.NewRequest(method, location, body)
	if e != nil {
		t.Fatal(e)
	}
	for k, v := range headers {
		if strings.EqualFold(k, "Host") {
			r.Host = v
		} else {
			r.Header.Set(k, v)
		}
	}
	response, e := c.Do(r)
	if e != nil {
		t.Fatal(e)
	}
	defer response.Body.Close()
	data, e := io.ReadAll(io.LimitReader(response.Body, 12*1024*1024+1))
	if e != nil {
		t.Fatal(e)
	}
	if len(data) > 12*1024*1024 {
		t.Fatal("response exceeds artifact budget")
	}
	return response, data
}
func TestManagedTransportMatrix(t *testing.T) {
	addr := managedFixture(t)
	for name, lane := range managedClients(t, addr) {
		t.Run(name, func(t *testing.T) {
			r, b := managedRequest(t, lane.client, "GET", lane.base+"/", nil, nil)
			if string(b) != "こんにちは λ" || r.StatusCode != 200 {
				t.Fatalf("ordinary: %d %q", r.StatusCode, b)
			}
			wantProto := 1
			if strings.HasPrefix(name, "h2") {
				wantProto = 2
			}
			if r.ProtoMajor != wantProto {
				t.Fatalf("wrong negotiated transport: %s", r.Proto)
			}
			_, b = managedRequest(t, lane.client, "GET", lane.base+"/stream", nil, nil)
			if string(b) != "こんにちは λ" {
				t.Fatalf("stream: %q", b)
			}
			r, b = managedRequest(t, lane.client, "HEAD", lane.base+"/", nil, nil)
			if len(b) != 0 || r.Header.Get("Content-Length") != "18" {
				t.Fatalf("HEAD: %v %q", r.Header, b)
			}
			r, b = managedRequest(t, lane.client, "GET", lane.base+"/empty", nil, nil)
			if r.StatusCode != 204 || len(b) != 0 {
				t.Fatal("204 body")
			}
			r, _ = managedRequest(t, lane.client, "GET", lane.base+"/cookies", nil, nil)
			if len(r.Header.Values("Set-Cookie")) != 2 {
				t.Fatalf("cookies: %v", r.Header)
			}
			binary := make([]byte, 65539)
			for i := range binary {
				binary[i] = byte(i % 251)
			}
			_, b = managedRequest(t, lane.client, "POST", lane.base+"/echo", bytes.NewReader(binary), map[string]string{"Content-Type": "application/octet-stream"})
			if !bytes.Equal(binary, b) {
				t.Fatal("binary request body mismatch")
			}
			_, b = managedRequest(t, lane.client, "GET", lane.base+"/mount/item", nil, nil)
			if string(b) != "/mount|/item" {
				t.Fatalf("mount: %q", b)
			}
			scheme := "http"
			if name == "https" || name == "h2tls" {
				scheme = "https"
			}
			_, b = managedRequest(t, lane.client, "GET", lane.base+"/env?token=fixture", nil, nil)
			if !strings.HasPrefix(string(b), scheme+"|127.0.0.1|/env|token=fixture") {
				t.Fatalf("env: %q", b)
			}
			_, portBody := managedRequest(t, lane.client, "GET", lane.base+"/env-port", nil, map[string]string{"Host": "localhost"})
			wantPort := "80"
			if scheme == "https" {
				wantPort = "443"
			}
			if string(portBody) != wantPort {
				t.Fatalf("default server-port: %q", portBody)
			}
			for _, path := range []string{"/", "/stream"} {
				r, b = managedRequest(t, lane.client, "GET", lane.base+path, nil, map[string]string{"Accept-Encoding": "gzip;q=1, zstd;q=0"})
				if r.Header.Get("Content-Encoding") != "gzip" {
					t.Fatalf("encoding %v", r.Header)
				}
				gz, e := gzip.NewReader(bytes.NewReader(b))
				if e != nil {
					t.Fatal(e)
				}
				plain, e := io.ReadAll(gz)
				gz.Close()
				if e != nil || string(plain) != "こんにちは λ" {
					t.Fatal("gzip streaming mismatch", e)
				}
			}
			r, encoded := managedRequest(t, lane.client, "GET", lane.base+"/stream", nil, map[string]string{"Accept-Encoding": "zstd;q=1, gzip;q=0"})
			if r.Header.Get("Content-Encoding") != "zstd" {
				t.Fatal("missing zstd encoding")
			}
			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
			decoder := exec.CommandContext(ctx, "zstd", "--decompress", "--stdout", "--quiet")
			decoder.Stdin = bytes.NewReader(encoded)
			decoded, err := decoder.Output()
			cancel()
			if err != nil || string(decoded) != "こんにちは λ" {
				t.Fatalf("zstd stream: %q %v", decoded, err)
			}
			r, _ = managedRequest(t, lane.client, "GET", lane.base+"/auth", nil, nil)
			if r.StatusCode != 401 {
				t.Fatal("auth missing")
			}
			r, _ = managedRequest(t, lane.client, "GET", lane.base+"/auth", nil, map[string]string{"Authorization": "Basic dGVzdDp0ZXN0"})
			if r.StatusCode != 200 {
				t.Fatal("auth accepted")
			}
			_, token := managedRequest(t, lane.client, "GET", lane.base+"/csrf", nil, nil)
			r, _ = managedRequest(t, lane.client, "POST", lane.base+"/csrf", strings.NewReader("_csrf_token="+url.QueryEscape(string(token))), map[string]string{"Content-Type": "application/x-www-form-urlencoded"})
			if r.StatusCode != 200 {
				t.Fatal("csrf token rejected")
			}
			r, _ = managedRequest(t, lane.client, "POST", lane.base+"/csrf", strings.NewReader("_csrf_token=wrong"), map[string]string{"Content-Type": "application/x-www-form-urlencoded"})
			if r.StatusCode != 400 {
				t.Fatal("csrf invalid accepted")
			}

			for _, path := range []string{"/file", "/static/fixture.txt"} {
				_, data := managedRequest(t, lane.client, "GET", lane.base+path, nil, nil)
				if string(data) != "Woo Lack fixture: こんにちは λ\n" {
					t.Fatalf("pathname/static: %q", data)
				}
			}
			_, data := managedRequest(t, lane.client, "GET", lane.base+"/db", nil, nil)
			if string(data) != "42" {
				t.Fatal("SQLite pool")
			}
			for _, path := range []string{"/session", "/sqlite", "/redis"} {
				jar, _ := cookiejar.New(nil)
				lane.client.Jar = jar
				_, data := managedRequest(t, lane.client, "GET", lane.base+path, nil, nil)
				if string(data) != "1" {
					t.Fatalf("new session %s: %q", path, data)
				}
				var group sync.WaitGroup
				for i := 0; i < 12; i++ {
					group.Add(1)
					go func() { defer group.Done(); managedRequest(t, lane.client, "GET", lane.base+path, nil, nil) }()
				}
				group.Wait()
				_, data = managedRequest(t, lane.client, "GET", lane.base+path, nil, nil)
				if string(data) != "14" {
					t.Fatalf("same-SID race %s: %q", path, data)
				}
			}
			_, b = managedRequest(t, lane.client, "GET", lane.base+"/large", nil, nil)
			if len(b) != 10*1024*1024 {
				t.Fatalf("large bounded pump: %d", len(b))
			}
		})
	}
}
func TestManagedHTTP1Pipeline(t *testing.T) {
	addr := managedFixture(t)
	conn, e := net.DialTimeout("tcp", addr, 5*time.Second)
	if e != nil {
		t.Fatal(e)
	}
	defer conn.Close()
	conn.SetDeadline(time.Now().Add(10 * time.Second))
	fmt.Fprint(conn, "GET /slow HTTP/1.1\r\nHost: localhost\r\n\r\nPOST /echo HTTP/1.1\r\nHost: localhost\r\nContent-Type: application/octet-stream\r\nTransfer-Encoding: chunked\r\n\r\n3\r\nabc\r\n0\r\n\r\nGET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	reader := bufio.NewReader(conn)
	for _, want := range []string{"ok", "abc", "こんにちは λ"} {
		r, e := http.ReadResponse(reader, &http.Request{Method: "GET"})
		if e != nil {
			t.Fatal(e)
		}
		b, e := io.ReadAll(r.Body)
		r.Body.Close()
		if e != nil || string(b) != want {
			t.Fatalf("pipeline: %q %v", b, e)
		}
	}
}
func TestManagedWebsocketDriver(t *testing.T) {
	addr := managedFixture(t)
	config, e := websocket.NewConfig("ws://"+addr+"/ws", "http://localhost/")
	if e != nil {
		t.Fatal(e)
	}
	config.Dialer = &net.Dialer{Timeout: 5 * time.Second}
	conn, e := websocket.DialConfig(config)
	if e != nil {
		t.Fatal(e)
	}
	defer conn.Close()
	conn.SetDeadline(time.Now().Add(5 * time.Second))
	if e = websocket.Message.Send(conn, "こんにちは λ"); e != nil {
		t.Fatal(e)
	}
	var message string
	if e = websocket.Message.Receive(conn, &message); e != nil || message != "こんにちは λ" {
		t.Fatalf("websocket: %q %v", message, e)
	}
}
func awaitManagedBaseline(t *testing.T, client *http.Client, base string) {
	t.Helper()
	deadline := time.Now().Add(5 * time.Second)
	for {
		_, metrics := managedRequest(t, client, "GET", base+"/metrics", nil, nil)
		if string(metrics) == "0|0|1" {
			return
		}
		if time.Now().After(deadline) {
			t.Fatalf("resources did not converge to baseline: %q", metrics)
		}
		time.Sleep(10 * time.Millisecond)
	}
}

func TestManagedSoak(t *testing.T) {
	seconds := 1800
	if value := os.Getenv("WOO_COMPAT_SOAK_SECONDS"); value != "" {
		n, e := strconv.Atoi(value)
		if e != nil || n < 1 || n > 1800 {
			t.Fatal("invalid soak duration")
		}
		seconds = n
	}
	addr := managedFixture(t)
	lanes := managedClients(t, addr)
	// Warm each transport before taking a descriptor/RSS baseline.
	for _, lane := range lanes {
		managedRequest(t, lane.client, "GET", lane.base+"/stream", nil, nil)
	}
	pid, ok := managedProcesses.Load(addr)
	if !ok {
		t.Fatal("missing fixture process identity")
	}
	baselineFDs, baselineRSS := managedResources(t, pid.(int))
	peakFDs, peakRSS := baselineFDs, baselineRSS
	sample := func() {
		fds, rss := managedResources(t, pid.(int))
		if fds > baselineFDs+8 {
			t.Fatalf("descriptor growth: baseline=%d current=%d", baselineFDs, fds)
		}
		if rss > baselineRSS+256*1024 {
			t.Fatalf("RSS growth exceeds 256 MiB budget: baseline=%d current=%d KiB", baselineRSS, rss)
		}
		peakFDs = max(peakFDs, fds)
		peakRSS = max(peakRSS, rss)
		t.Logf("soak sample: descriptors=%d RSS_KiB=%d", fds, rss)
	}
	nextSample := time.Now().Add(30 * time.Second)
	deadline := time.Now().Add(time.Duration(seconds) * time.Second)
	count := 0
	for time.Now().Before(deadline) {
		for _, lane := range lanes {
			_, b := managedRequest(t, lane.client, "GET", lane.base+"/stream", nil, nil)
			if string(b) != "こんにちは λ" {
				t.Fatal("soak response mismatch")
			}
			count++
			if count%100 == 0 {
				awaitManagedBaseline(t, lane.client, lane.base)
			}
		}
		if time.Now().After(nextSample) {
			sample()
			nextSample = time.Now().Add(30 * time.Second)
		}
		time.Sleep(10 * time.Millisecond)
	}
	sample()
	t.Logf("soak completed: seconds=%d requests=%d descriptors=%d..%d RSS_KiB=%d..%d", seconds, count, baselineFDs, peakFDs, baselineRSS, peakRSS)
}

func managedResources(t *testing.T, pid int) (int, int) {
	t.Helper()
	run := func(name string, args ...string) string {
		ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
		defer cancel()
		output, err := exec.CommandContext(ctx, name, args...).Output()
		if err != nil {
			t.Fatalf("resource sampler %s: %v", name, err)
		}
		return string(output)
	}
	fds := 0
	for _, line := range strings.Split(run("lsof", "-a", "-p", strconv.Itoa(pid), "-d", "0-99999", "-Ff"), "\n") {
		if strings.HasPrefix(line, "f") {
			fds++
		}
	}
	rss, err := strconv.Atoi(strings.TrimSpace(run("ps", "-o", "rss=", "-p", strconv.Itoa(pid))))
	if err != nil || fds == 0 || rss <= 0 {
		t.Fatal("invalid fixture resource sample")
	}
	return fds, rss
}

func TestManagedBudgetAndCancellation(t *testing.T) {
	addr := managedFixture(t)
	for name, lane := range managedClients(t, addr) {
		t.Run(name, func(t *testing.T) {
			for _, path := range []string{"/budget", "/broken-stream"} {
				response, err := lane.client.Get(lane.base + path)
				if err == nil {
					_, err = io.Copy(io.Discard, response.Body)
					response.Body.Close()
				}
				if err == nil {
					t.Fatalf("failed streaming response was not cancelled: %s", path)
				}
			}
			request, err := http.NewRequest("POST", lane.base+"/echo", bytes.NewReader(make([]byte, 131073)))
			if err != nil {
				t.Fatal(err)
			}
			if !strings.HasPrefix(name, "h2") {
				// The server rejects this from Content-Length in the header
				// callback. Wait for that response before uploading the body so
				// the assertion observes the HTTP contract instead of racing a
				// client-side write against the server's intentional close.
				request.Header.Set("Expect", "100-continue")
			}
			response, err := lane.client.Do(request)
			if err == nil {
				_, readErr := io.Copy(io.Discard, response.Body)
				response.Body.Close()
				if readErr != nil {
					t.Fatal("oversize response did not finish:", readErr)
				}
				if response.StatusCode != http.StatusRequestEntityTooLarge {
					t.Fatalf("oversize body admitted: %d", response.StatusCode)
				}
			} else if !strings.HasPrefix(name, "h2") {
				t.Fatal("HTTP/1 body limit did not return 413:", err)
			}
			// A cancelled H2 stream must not poison another stream or retain its budget.
			awaitManagedBaseline(t, lane.client, lane.base)
			_, body := managedRequest(t, lane.client, "GET", lane.base+"/", nil, nil)
			if string(body) != "こんにちは λ" {
				t.Fatal("request after cancellation failed")
			}
		})
	}
}

func TestManagedHTTP2Drain(t *testing.T) {
	addr := managedFixture(t)
	lanes := managedClients(t, addr)
	responses := make(map[string]*http.Response)
	for _, name := range []string{"h2c", "h2tls"} {
		response, err := lanes[name].client.Get(lanes[name].base + "/drain-slow")
		if err != nil {
			t.Fatal(err)
		}
		prefix := make([]byte, 6)
		if _, err := io.ReadFull(response.Body, prefix); err != nil || string(prefix) != "before" {
			response.Body.Close()
			t.Fatal("active stream prefix", err)
		}
		responses[name] = response
		t.Cleanup(func() { response.Body.Close() })
	}
	_, body := managedRequest(t, lanes["http1"].client, "GET", lanes["http1"].base+"/drain", nil, nil)
	if string(body) != "draining" {
		t.Fatal("shutdown trigger")
	}
	for name, response := range responses {
		body, err := io.ReadAll(response.Body)
		response.Body.Close()
		if err != nil || string(body) != "after" {
			t.Fatalf("active %s stream lost during drain: %q %v", name, body, err)
		}
	}
	// Drain must close both listeners and complete workers within the deadline.
	deadline := time.Now().Add(5 * time.Second)
	for _, name := range []string{"h2c", "h2tls"} {
		for {
			response, err := lanes[name].client.Get(lanes[name].base + "/")
			if err != nil {
				break
			}
			response.Body.Close()
			if time.Now().After(deadline) {
				t.Fatal("listener still admitted requests after drain")
			}
			time.Sleep(10 * time.Millisecond)
		}
	}
}
