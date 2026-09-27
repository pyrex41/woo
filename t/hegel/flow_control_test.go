package hegeltest

import (
	"bytes"
	"io"
	"net"
	"strconv"
	"testing"
	"time"

	"golang.org/x/net/http2"
	"golang.org/x/net/http2/hpack"
	"hegel.dev/go/hegel"
)

// Observe actual stalls and resumptions: the Go HTTP client normally grants
// enough response credit to hide both of these boundaries for a 70 KB body.
func TestHTTP2ResponseWaitsForStreamAndConnectionCredit(t *testing.T) {
	addr := startWoo(t)
	hegel.Test(t, func(ht *hegel.T) {
		firstCredit := hegel.Draw(ht, hegel.Integers(1, 1024))
		expected := make([]byte, 70000)
		for i := range expected {
			expected[i] = byte(i % 251)
		}
		conn, err := net.DialTimeout("tcp", addr, 2*time.Second)
		if err != nil {
			ht.Fatal(err)
		}
		defer conn.Close()
		conn.SetDeadline(time.Now().Add(5 * time.Second))
		if _, err := io.WriteString(conn, http2.ClientPreface); err != nil {
			ht.Fatal(err)
		}
		framer := http2.NewFramer(conn, conn)
		framer.ReadMetaHeaders = hpack.NewDecoder(4096, nil)
		if err := framer.WriteSettings(http2.Setting{ID: http2.SettingInitialWindowSize, Val: 0}); err != nil {
			ht.Fatal(err)
		}
		var block bytes.Buffer
		encoder := hpack.NewEncoder(&block)
		for _, field := range []hpack.HeaderField{
			{Name: ":method", Value: "GET"}, {Name: ":scheme", Value: "http"},
			{Name: ":authority", Value: addr}, {Name: ":path", Value: "/flow-body"},
		} {
			if err := encoder.WriteField(field); err != nil {
				ht.Fatal(err)
			}
		}
		if err := framer.WriteHeaders(http2.HeadersFrameParam{StreamID: 1, BlockFragment: block.Bytes(), EndHeaders: true, EndStream: true}); err != nil {
			ht.Fatal(err)
		}
		headers, settingsAck := false, false
		for !headers || !settingsAck {
			frame, err := framer.ReadFrame()
			if err != nil {
				ht.Fatal(err)
			}
			switch frame := frame.(type) {
			case *http2.SettingsFrame:
				if frame.IsAck() {
					settingsAck = true
				} else if err := framer.WriteSettingsAck(); err != nil {
					ht.Fatal(err)
				}
			case *http2.MetaHeadersFrame:
				if frame.StreamID != 1 || frame.PseudoValue("status") != "200" || frame.StreamEnded() {
					ht.Fatalf("unexpected response headers: %+v", frame)
				}
				headers = true
			default:
				ht.Fatalf("unexpected frame before credit: %T", frame)
			}
		}
		assertNoResponseBytes(ht, conn)
		if err := framer.WriteWindowUpdate(1, uint32(firstCredit)); err != nil {
			ht.Fatal(err)
		}
		readFlowBytes(ht, framer, expected[:firstCredit], false)
		assertNoResponseBytes(ht, conn)

		// Grant all remaining stream credit. Connection credit is still only
		// the initial 65,535 bytes: Woo must stop there despite stream credit.
		if err := framer.WriteWindowUpdate(1, uint32(70000-firstCredit)); err != nil {
			ht.Fatal(err)
		}
		readFlowBytes(ht, framer, expected[firstCredit:65535], false)
		assertNoResponseBytes(ht, conn)
		if err := framer.WriteWindowUpdate(0, 70000-65535); err != nil {
			ht.Fatal(err)
		}
		readFlowBytes(ht, framer, expected[65535:], true)
	}, hegel.WithTestCases(12))
}

// Leave stream 1 open while stream 3 completes and receives its response.
// This requires the server to keep both request streams live at once.
func TestHTTP2OverlappingStreams(t *testing.T) {
	addr := startWoo(t)
	hegel.Test(t, func(ht *hegel.T) {
		ids := []int{hegel.Draw(ht, hegel.Integers(0, 9999)), hegel.Draw(ht, hegel.Integers(0, 9999))}
		conn, err := net.DialTimeout("tcp", addr, 2*time.Second)
		if err != nil {
			ht.Fatal(err)
		}
		defer conn.Close()
		conn.SetDeadline(time.Now().Add(5 * time.Second))
		if _, err := io.WriteString(conn, http2.ClientPreface); err != nil {
			ht.Fatal(err)
		}
		framer := http2.NewFramer(conn, conn)
		framer.ReadMetaHeaders = hpack.NewDecoder(4096, nil)
		if err := framer.WriteSettings(); err != nil {
			ht.Fatal(err)
		}
		paths := map[uint32]string{}
		var block bytes.Buffer
		encoder := hpack.NewEncoder(&block)
		for i, id := range ids {
			streamID := uint32(1 + 2*i)
			path := "/echo/overlap-" + strconv.Itoa(i) + "-" + strconv.Itoa(id)
			paths[streamID] = path
			block.Reset()
			for _, field := range []hpack.HeaderField{
				{Name: ":method", Value: "GET"}, {Name: ":scheme", Value: "http"},
				{Name: ":authority", Value: addr}, {Name: ":path", Value: path},
			} {
				if err := encoder.WriteField(field); err != nil {
					ht.Fatal(err)
				}
			}
			if err := framer.WriteHeaders(http2.HeadersFrameParam{StreamID: streamID, BlockFragment: block.Bytes(), EndHeaders: true, EndStream: streamID == 3}); err != nil {
				ht.Fatal(err)
			}
		}
		bodies := map[uint32][]byte{}
		headers := map[uint32]bool{}
		ended := map[uint32]bool{}
		readResponseFrame := func() {
			frame, err := framer.ReadFrame()
			if err != nil {
				ht.Fatal(err)
			}
			switch frame := frame.(type) {
			case *http2.SettingsFrame:
				if !frame.IsAck() {
					if err := framer.WriteSettingsAck(); err != nil {
						ht.Fatal(err)
					}
				}
			case *http2.MetaHeadersFrame:
				if paths[frame.StreamID] == "" || frame.PseudoValue("status") != "200" || headers[frame.StreamID] {
					ht.Fatalf("unexpected response headers: %+v", frame)
				}
				headers[frame.StreamID] = true
				if frame.StreamEnded() {
					ended[frame.StreamID] = true
				}
			case *http2.DataFrame:
				if !headers[frame.StreamID] || ended[frame.StreamID] {
					ht.Fatalf("unexpected DATA on stream %d", frame.StreamID)
				}
				bodies[frame.StreamID] = append(bodies[frame.StreamID], frame.Data()...)
				if frame.StreamEnded() {
					ended[frame.StreamID] = true
				}
			default:
				ht.Fatalf("unexpected frame while streams overlap: %T", frame)
			}
		}
		for !ended[3] {
			readResponseFrame()
			if headers[1] {
				ht.Fatal("stream 1 responded before its request ended")
			}
		}
		if err := framer.WriteData(1, true, nil); err != nil {
			ht.Fatal(err)
		}
		for !ended[1] {
			readResponseFrame()
		}
		for streamID, path := range paths {
			if !bytes.Equal(bodies[streamID], []byte(path)) {
				ht.Fatalf("stream %d body=%q want=%q", streamID, bodies[streamID], path)
			}
		}
	}, hegel.WithTestCases(20))
}

func assertNoResponseBytes(ht *hegel.T, conn net.Conn) {
	ht.Helper()
	conn.SetReadDeadline(time.Now().Add(40 * time.Millisecond))
	var octet [1]byte
	n, err := conn.Read(octet[:])
	if timeout, ok := err.(net.Error); n != 0 || !ok || !timeout.Timeout() {
		ht.Fatalf("response did not wait for credit: bytes=%x error=%v", octet[:n], err)
	}
	conn.SetReadDeadline(time.Now().Add(5 * time.Second))
}

func readFlowBytes(ht *hegel.T, framer *http2.Framer, expected []byte, endStream bool) {
	ht.Helper()
	for len(expected) > 0 {
		frame, err := framer.ReadFrame()
		if err != nil {
			ht.Fatal(err)
		}
		data, ok := frame.(*http2.DataFrame)
		if !ok {
			ht.Fatalf("expected DATA, got %T", frame)
		}
		if data.StreamID != 1 || len(data.Data()) == 0 || len(data.Data()) > len(expected) {
			ht.Fatalf("invalid DATA at remaining credit %d: %+v", len(expected), data)
		}
		if !bytes.Equal(data.Data(), expected[:len(data.Data())]) {
			ht.Fatal("response body changed")
		}
		expected = expected[len(data.Data()):]
		if data.StreamEnded() != (endStream && len(expected) == 0) {
			ht.Fatalf("unexpected END_STREAM with %d bytes remaining", len(expected))
		}
	}
}
