package hegeltest

import (
	"bufio"
	"bytes"
	"crypto/sha1"
	"encoding/base64"
	"encoding/binary"
	"fmt"
	"io"
	"net"
	"net/http"
	"testing"
	"time"

	"golang.org/x/net/http2"
	"hegel.dev/go/hegel"
)

// This suite is intentionally outside woo-test.asd: Hegel generates and shrinks
// inputs in Go, while every assertion observes a live Woo process over TCP.

func TestHTTP1PipelinedRequests(t *testing.T) {
	addr := startWoo(t)
	hegel.Test(t, func(ht *hegel.T) {
		ids := hegel.Draw(ht, hegel.Lists(hegel.Integers(0, 9999)).MinSize(1).MaxSize(16))
		body := hegel.Draw(ht, hegel.Binary(0, 256))
		// Include a fake request delimiter inside a body. It must not be read
		// as the start of the next pipelined request.
		body = append([]byte("\r\n\r\nGET /echo/fake HTTP/1.1\r\n"), body...)
		var batch bytes.Buffer
		want := make([][]byte, 0, len(ids)+1)
		for i, id := range ids {
			path := fmt.Sprintf("/echo/%d-%d", i, id)
			if i == 0 {
				fmt.Fprintf(&batch, "POST /body HTTP/1.1\r\nHost: localhost\r\nContent-Length: %d\r\n\r\n", len(body))
				batch.Write(body)
				want = append(want, body)
			} else {
				fmt.Fprintf(&batch, "GET %s HTTP/1.1\r\nHost: localhost\r\n\r\n", path)
				want = append(want, []byte(path))
			}
		}
		// A final request exposes a parser that mistakenly consumes or drops
		// bytes after a fixed-length body or a batch of bodiless requests.
		batch.WriteString("GET /echo/final HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
		want = append(want, []byte("/echo/final"))

		conn, err := net.DialTimeout("tcp", addr, 2*time.Second)
		if err != nil {
			ht.Fatal(err)
		}
		defer conn.Close()
		conn.SetDeadline(time.Now().Add(5 * time.Second))
		split := hegel.Draw(ht, hegel.Integers(0, batch.Len()))
		if _, err := conn.Write(batch.Bytes()[:split]); err != nil {
			ht.Fatal(err)
		}
		if _, err := conn.Write(batch.Bytes()[split:]); err != nil {
			ht.Fatal(err)
		}
		reader := bufio.NewReader(conn)
		for i, expected := range want {
			resp, err := http.ReadResponse(reader, nil)
			if err != nil {
				ht.Fatalf("response %d: %v", i, err)
			}
			got, err := io.ReadAll(io.LimitReader(resp.Body, 4096))
			resp.Body.Close()
			if err != nil || resp.StatusCode != 200 || !bytes.Equal(got, expected) {
				ht.Fatalf("response %d: status=%d body=%x want=%x error=%v", i, resp.StatusCode, got, expected, err)
			}
		}
	}, hegel.WithTestCases(60))
}

func TestHTTP2SplitPrefaceAndBatchedPings(t *testing.T) {
	addr := startWoo(t)
	hegel.Test(t, func(ht *hegel.T) {
		split := hegel.Draw(ht, hegel.Integers(1, len(http2.ClientPreface)-1))
		count := hegel.Draw(ht, hegel.Integers(1, 12))
		payload := hegel.Draw(ht, hegel.Binary(8, 8))
		conn, err := net.DialTimeout("tcp", addr, 2*time.Second)
		if err != nil {
			ht.Fatal(err)
		}
		defer conn.Close()
		conn.SetDeadline(time.Now().Add(5 * time.Second))
		if _, err := io.WriteString(conn, http2.ClientPreface[:split]); err != nil {
			ht.Fatal(err)
		}
		var tail bytes.Buffer
		tail.WriteString(http2.ClientPreface[split:])
		framer := http2.NewFramer(&tail, nil)
		if err := framer.WriteSettings(); err != nil {
			ht.Fatal(err)
		}
		var ping [8]byte
		copy(ping[:], payload)
		for i := 0; i < count; i++ {
			ping[0] = byte(i)
			if err := framer.WritePing(false, ping); err != nil {
				ht.Fatal(err)
			}
		}
		if _, err := conn.Write(tail.Bytes()); err != nil {
			ht.Fatal(err)
		}
		reader := http2.NewFramer(nil, conn)
		seen := 0
		for frames := 0; frames < count+5 && seen < count; frames++ {
			frame, err := reader.ReadFrame()
			if err != nil {
				ht.Fatalf("read after %d/%d ping ACKs: %v", seen, count, err)
			}
			if ack, ok := frame.(*http2.PingFrame); ok && ack.IsAck() {
				ping[0] = byte(seen)
				if ack.Data != ping {
					ht.Fatalf("ping ACK %d: got %x, want %x", seen, ack.Data, ping)
				}
				seen++
			}
		}
		if seen != count {
			ht.Fatalf("received %d of %d ping ACKs", seen, count)
		}
	}, hegel.WithTestCases(60))
}

// Pin each generated history to a fresh TCP connection. ClientConn does not
// silently dial another connection or retry on one after GOAWAY/disconnect.
func oneHTTP2Client(addr string) (*http.Client, func(), error) {
	conn, err := net.DialTimeout("tcp", addr, 2*time.Second)
	if err != nil {
		return nil, nil, err
	}
	transport := &http2.Transport{}
	cc, err := transport.NewClientConn(conn)
	if err != nil {
		conn.Close()
		return nil, nil, err
	}
	return &http.Client{Transport: cc, Timeout: 8 * time.Second}, func() { cc.Close() }, nil
}

func TestHTTP2RequestsOnOneConnection(t *testing.T) {
	addr := startWoo(t)
	hegel.Test(t, func(ht *hegel.T) {
		client, closeClient, err := oneHTTP2Client(addr)
		if err != nil {
			ht.Fatal(err)
		}
		defer closeClient()
		ids := hegel.Draw(ht, hegel.Lists(hegel.Integers(0, 9999)).MinSize(2).MaxSize(12))
		for i, id := range ids {
			path := fmt.Sprintf("/echo/%d-%d", i, id)
			resp, err := client.Get("http://" + addr + path)
			if err != nil {
				ht.Fatalf("GET %s: %v", path, err)
			}
			got, readErr := io.ReadAll(io.LimitReader(resp.Body, 4096))
			resp.Body.Close()
			if readErr != nil || resp.ProtoMajor != 2 || resp.StatusCode != 200 || string(got) != path {
				ht.Fatalf("GET %s: proto=%s status=%d body=%q error=%v", path, resp.Proto, resp.StatusCode, got, readErr)
			}
		}
	}, hegel.WithTestCases(40))
}

func TestHTTP2RequestAndResponseBodies(t *testing.T) {
	addr := startWoo(t)
	hegel.Test(t, func(ht *hegel.T) {
		client, closeClient, err := oneHTTP2Client(addr)
		if err != nil {
			ht.Fatal(err)
		}
		defer closeClient()
		// Exercise replenishment of Woo's request receive window. Response
		// backpressure is tested separately with explicitly controlled credit.
		body := hegel.Draw(ht, hegel.Binary(65_536, 80_000))
		resp, err := client.Post("http://"+addr+"/body", "application/octet-stream", bytes.NewReader(body))
		if err != nil {
			ht.Fatalf("POST %d bytes: %v", len(body), err)
		}
		got, readErr := io.ReadAll(io.LimitReader(resp.Body, 80_001))
		resp.Body.Close()
		if readErr != nil || resp.ProtoMajor != 2 || resp.StatusCode != 200 || !bytes.Equal(got, body) {
			ht.Fatalf("POST %d bytes: proto=%s status=%d response=%d bytes error=%v", len(body), resp.Proto, resp.StatusCode, len(got), readErr)
		}
	}, hegel.WithTestCases(25))
}

func TestHTTP2LargeOrdinaryResponse(t *testing.T) {
	addr := startWoo(t)
	client, closeClient, err := oneHTTP2Client(addr)
	if err != nil {
		t.Fatal(err)
	}
	defer closeClient()
	client.Timeout = 30 * time.Second
	resp, err := client.Get("http://" + addr + "/large-body")
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()
	const size = 8*1024*1024 + 1
	got, err := io.ReadAll(io.LimitReader(resp.Body, size+1))
	if err != nil {
		t.Fatal(err)
	}
	if resp.ProtoMajor != 2 || resp.StatusCode != 200 || len(got) != size || !bytes.Equal(got, bytes.Repeat([]byte{'a'}, size)) {
		t.Fatalf("large ordinary response: proto=%s status=%d bytes=%d want=%d", resp.Proto, resp.StatusCode, len(got), size)
	}
}

func maskedBinaryFrame(payload, mask []byte) []byte {
	frame := []byte{0x82}
	switch {
	case len(payload) < 126:
		frame = append(frame, 0x80|byte(len(payload)))
	case len(payload) <= 65535:
		frame = append(frame, 0x80|126, byte(len(payload)>>8), byte(len(payload)))
	default:
		panic("test payload exceeds 65535 bytes")
	}
	frame = append(frame, mask...)
	for i, b := range payload {
		frame = append(frame, b^mask[i%4])
	}
	return frame
}

func readServerBinaryFrame(reader *bufio.Reader) ([]byte, error) {
	var head [2]byte
	if _, err := io.ReadFull(reader, head[:]); err != nil {
		return nil, err
	}
	if head[0] != 0x82 || head[1]&0x80 != 0 {
		return nil, fmt.Errorf("unexpected WebSocket frame header %x", head)
	}
	n := uint64(head[1] & 0x7f)
	if n == 126 {
		var ext [2]byte
		if _, err := io.ReadFull(reader, ext[:]); err != nil {
			return nil, err
		}
		n = uint64(binary.BigEndian.Uint16(ext[:]))
	}
	if n > 4096 {
		return nil, fmt.Errorf("WebSocket response exceeds test limit: %d", n)
	}
	data := make([]byte, n)
	_, err := io.ReadFull(reader, data)
	return data, err
}

func TestWebSocketUpgradeBehindPOSTBody(t *testing.T) {
	addr := startWoo(t)
	hegel.Test(t, func(ht *hegel.T) {
		body := hegel.Draw(ht, hegel.Binary(1, 128))
		payload := hegel.Draw(ht, hegel.Binary(0, 300))
		mask := hegel.Draw(ht, hegel.Binary(4, 4))
		keyBytes := hegel.Draw(ht, hegel.Binary(16, 16))
		key := base64.StdEncoding.EncodeToString(keyBytes)
		var batch bytes.Buffer
		fmt.Fprintf(&batch, "POST /body HTTP/1.1\r\nHost: localhost\r\nContent-Length: %d\r\n\r\n", len(body))
		batch.Write(body)
		fmt.Fprintf(&batch, "GET /ws HTTP/1.1\r\nHost: localhost\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: %s\r\nSec-WebSocket-Version: 13\r\n\r\n", key)
		batch.Write(maskedBinaryFrame(payload, mask))
		conn, err := net.DialTimeout("tcp", addr, 2*time.Second)
		if err != nil {
			ht.Fatal(err)
		}
		defer conn.Close()
		conn.SetDeadline(time.Now().Add(5 * time.Second))
		split := hegel.Draw(ht, hegel.Integers(0, batch.Len()))
		if _, err := conn.Write(batch.Bytes()[:split]); err != nil {
			ht.Fatal(err)
		}
		if _, err := conn.Write(batch.Bytes()[split:]); err != nil {
			ht.Fatal(err)
		}
		reader := bufio.NewReader(conn)
		first, err := http.ReadResponse(reader, nil)
		if err != nil {
			ht.Fatalf("read POST response: %v", err)
		}
		got, readErr := io.ReadAll(io.LimitReader(first.Body, 4096))
		first.Body.Close()
		if readErr != nil || first.StatusCode != 200 || !bytes.Equal(got, body) {
			ht.Fatalf("POST response: status=%d body=%x want=%x error=%v", first.StatusCode, got, body, readErr)
		}
		upgrade, err := http.ReadResponse(reader, nil)
		if err != nil {
			ht.Fatalf("read upgrade response: %v", err)
		}
		defer upgrade.Body.Close()
		accept := sha1.Sum([]byte(key + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"))
		wantAccept := base64.StdEncoding.EncodeToString(accept[:])
		if upgrade.StatusCode != 101 || upgrade.Header.Get("Sec-WebSocket-Accept") != wantAccept {
			ht.Fatalf("upgrade: status=%d accept=%q, want %q", upgrade.StatusCode, upgrade.Header.Get("Sec-WebSocket-Accept"), wantAccept)
		}
		echo, err := readServerBinaryFrame(reader)
		if err != nil || !bytes.Equal(echo, payload) {
			ht.Fatalf("WebSocket echo: got=%x want=%x error=%v", echo, payload, err)
		}
	}, hegel.WithTestCases(40))
}
