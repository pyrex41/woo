package hegeltest

import (
	"bufio"
	"bytes"
	"crypto/sha1"
	"encoding/base64"
	"fmt"
	"io"
	"net"
	"net/http"
	"strconv"
	"strings"
	"testing"
	"time"

	"hegel.dev/go/hegel"
)

type parityResult struct {
	status int
	proto  int
	body   []byte
}

func parityClient(addr string, h2 bool) (*http.Client, func(), error) {
	if h2 {
		return oneHTTP2Client(addr)
	}
	transport := &http.Transport{MaxIdleConnsPerHost: 2}
	return &http.Client{Transport: transport, Timeout: 8 * time.Second}, transport.CloseIdleConnections, nil
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
			hegel.Test(t, func(ht *hegel.T) {
				wooClient, closeWoo, err := parityClient(woo, protocol.h2)
				if err != nil {
					ht.Fatal(err)
				}
				defer closeWoo()
				refClient, closeRef, err := parityClient(reference, protocol.h2)
				if err != nil {
					ht.Fatal(err)
				}
				defer closeRef()
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

func TestReferenceParityValidRoutes(t *testing.T) {
	woo := startWoo(t)
	reference := startOracle(t)
	for _, h2 := range []bool{false, true} {
		t.Run(map[bool]string{false: "http1", true: "h2c"}[h2], func(t *testing.T) {
			wooClient, closeWoo, err := parityClient(woo, h2)
			if err != nil {
				t.Fatal(err)
			}
			defer closeWoo()
			refClient, closeRef, err := parityClient(reference, h2)
			if err != nil {
				t.Fatal(err)
			}
			defer closeRef()
			hegel.Test(t, func(ht *hegel.T) {
				for _, code := range []int{200, 425, 428, 429, 431, 500, 511} {
					checkParityRequest(ht, wooClient, refClient, woo, reference, http.MethodGet,
						fmt.Sprintf("/status/%d", code), nil, h2, []byte(strconv.Itoa(code)))
				}
				checkParityRequest(ht, wooClient, refClient, woo, reference, http.MethodGet,
					"/static/fixture.txt", nil, h2, []byte("woo reference fixture\n"))
				payload := bytes.Repeat([]byte("upload"), 512)
				checkParityRequest(ht, wooClient, refClient, woo, reference, http.MethodPost,
					"/upload", payload, h2, payload)
			}, hegel.WithTestCases(1))
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
	wantStatus := http.StatusOK
	if strings.HasPrefix(path, "/status/") {
		var statusErr error
		wantStatus, statusErr = strconv.Atoi(strings.TrimPrefix(path, "/status/"))
		if statusErr != nil {
			ht.Fatalf("invalid status fixture path %q: %v", path, statusErr)
		}
	}
	wantProto := 1
	if h2 {
		wantProto = 2
	}
	if left.status != wantStatus || right.status != wantStatus || left.proto != wantProto || right.proto != wantProto ||
		!bytes.Equal(left.body, expected) || !bytes.Equal(right.body, expected) ||
		!bytes.Equal(left.body, right.body) {
		ht.Fatalf("%s %s: Woo=%+v reference=%+v wantStatus=%d wantProto=%d wantBody=%x",
			method, path, left, right, wantStatus, wantProto, expected)
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
