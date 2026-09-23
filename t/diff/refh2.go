// Differential oracle: Go's HTTP/2 stack (golang.org/x/net/http2).
// One request per stdin line, exactly one answer line per request. Fields are
// hex; "-" stands for an empty field.
//
//	ENC <namehex> <valuehex> ...  -> ENC <blockhex> | ENC ERR
//	DEC <blockhex>                 -> DEC <namehex> <valuehex> ... | DEC ERR
//	FRM <framehex>                 -> FRM OK <type> <flags> <sid> <length> <payloadhex> | FRM SHORT | FRM ERR
//	ENCSEQ <fields> / <fields> ... -> ENCSEQ <blockhex> <blockhex> ... | ENCSEQ ERR
//	DECSEQ <blockhex> ...          -> DECSEQ <fields> / <fields> ... | DECSEQ ERR
//
// ENCSEQ and DECSEQ reuse ONE hpack encoder or decoder across all blocks on
// the line, so later blocks depend on the dynamic table built by earlier ones.
//
// The FRM payload is Go's semantic view of the frame written back out by
// Go's own Framer (header stripped), so it compares what Go understood, not
// just the input bytes.
package main

import (
	"bufio"
	"bytes"
	"encoding/hex"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"

	"golang.org/x/net/http2"
	"golang.org/x/net/http2/hpack"
)

func unhex(s string) ([]byte, error) {
	if s == "" || s == "-" {
		return []byte{}, nil
	}
	return hex.DecodeString(s)
}

func tohex(b []byte) string {
	if len(b) == 0 {
		return "-"
	}
	return hex.EncodeToString(b)
}

func parseFields(parts []string) ([]hpack.HeaderField, bool) {
	if len(parts)%2 != 0 {
		return nil, false
	}
	var fields []hpack.HeaderField
	for i := 0; i < len(parts); i += 2 {
		name, err1 := unhex(parts[i])
		value, err2 := unhex(parts[i+1])
		if err1 != nil || err2 != nil {
			return nil, false
		}
		fields = append(fields, hpack.HeaderField{Name: string(name), Value: string(value)})
	}
	return fields, true
}

func encodeWith(enc *hpack.Encoder, buf *bytes.Buffer, fields []hpack.HeaderField) (string, bool) {
	buf.Reset()
	for _, f := range fields {
		if err := enc.WriteField(f); err != nil {
			return "", false
		}
	}
	return tohex(buf.Bytes()), true
}

func encodeFields(fields []hpack.HeaderField) string {
	var buf bytes.Buffer
	block, ok := encodeWith(hpack.NewEncoder(&buf), &buf, fields)
	if !ok {
		return "ENC ERR"
	}
	return "ENC " + block
}

func writeFields(b *strings.Builder, hs []hpack.HeaderField) {
	for _, h := range hs {
		fmt.Fprintf(b, " %s %s", tohex([]byte(h.Name)), tohex([]byte(h.Value)))
	}
}

func decodeBlock(raw []byte) string {
	dec := hpack.NewDecoder(4096, nil)
	hs, err := dec.DecodeFull(raw)
	if err != nil {
		return "DEC ERR"
	}
	var b strings.Builder
	b.WriteString("DEC")
	writeFields(&b, hs)
	return b.String()
}

// splitBlocks splits tokens on "/" into per-block groups.
func splitBlocks(parts []string) [][]string {
	groups := [][]string{{}}
	for _, p := range parts {
		if p == "/" {
			groups = append(groups, []string{})
			continue
		}
		groups[len(groups)-1] = append(groups[len(groups)-1], p)
	}
	return groups
}

func encodeSeq(parts []string) string {
	var buf bytes.Buffer
	enc := hpack.NewEncoder(&buf)
	var b strings.Builder
	b.WriteString("ENCSEQ")
	for _, group := range splitBlocks(parts) {
		fields, ok := parseFields(group)
		if !ok {
			return "ENCSEQ ERR"
		}
		block, ok := encodeWith(enc, &buf, fields)
		if !ok {
			return "ENCSEQ ERR"
		}
		b.WriteString(" " + block)
	}
	return b.String()
}

func decodeSeq(parts []string) string {
	dec := hpack.NewDecoder(4096, nil)
	var b strings.Builder
	b.WriteString("DECSEQ")
	for i, p := range parts {
		raw, err := unhex(p)
		if err != nil {
			return "DECSEQ ERR"
		}
		hs, err := dec.DecodeFull(raw)
		if err != nil {
			return "DECSEQ ERR"
		}
		if i > 0 {
			b.WriteString(" /")
		}
		writeFields(&b, hs)
	}
	return b.String()
}

// canonicalPayload writes Go's parsed view of f back out with Go's Framer
// and returns the payload (no 9-byte header).
func canonicalPayload(f http2.Frame) ([]byte, error) {
	var out bytes.Buffer
	w := http2.NewFramer(&out, nil)
	w.AllowIllegalWrites = true
	sid := f.Header().StreamID
	var err error
	switch f := f.(type) {
	case *http2.DataFrame:
		err = w.WriteData(sid, f.StreamEnded(), f.Data())
	case *http2.HeadersFrame:
		p := http2.HeadersFrameParam{
			StreamID:      sid,
			BlockFragment: f.HeaderBlockFragment(),
			EndStream:     f.StreamEnded(),
			EndHeaders:    f.HeadersEnded(),
		}
		if f.HasPriority() {
			p.Priority = f.Priority
		}
		err = w.WriteHeaders(p)
	case *http2.SettingsFrame:
		if f.IsAck() {
			err = w.WriteSettingsAck()
		} else {
			var ss []http2.Setting
			f.ForeachSetting(func(s http2.Setting) error {
				ss = append(ss, s)
				return nil
			})
			err = w.WriteSettings(ss...)
		}
	case *http2.WindowUpdateFrame:
		err = w.WriteWindowUpdate(sid, f.Increment)
	case *http2.PingFrame:
		err = w.WritePing(f.IsAck(), f.Data)
	case *http2.GoAwayFrame:
		err = w.WriteGoAway(f.LastStreamID, f.ErrCode, f.DebugData())
	case *http2.RSTStreamFrame:
		err = w.WriteRSTStream(sid, f.ErrCode)
	default:
		return nil, fmt.Errorf("unsupported frame %T", f)
	}
	if err != nil {
		return nil, err
	}
	return out.Bytes()[9:], nil
}

func decodeFrame(raw []byte) string {
	fr := http2.NewFramer(nil, bytes.NewReader(raw))
	fr.SetMaxReadFrameSize(1<<24 - 1)
	f, err := fr.ReadFrame()
	if err != nil {
		if err == io.EOF || err == io.ErrUnexpectedEOF {
			return "FRM SHORT"
		}
		return "FRM ERR"
	}
	h := f.Header()
	payload, err := canonicalPayload(f)
	if err != nil {
		return "FRM ERR"
	}
	return fmt.Sprintf("FRM OK %d %d %d %d %s", h.Type, h.Flags, h.StreamID, h.Length, tohex(payload))
}

func handle(line string) string {
	parts := strings.Fields(line)
	if len(parts) == 0 {
		return "ERR"
	}
	switch parts[0] {
	case "ENC":
		fields, ok := parseFields(parts[1:])
		if !ok {
			return "ENC ERR"
		}
		return encodeFields(fields)
	case "ENCSEQ":
		return encodeSeq(parts[1:])
	case "DECSEQ":
		if len(parts) < 2 {
			return "DECSEQ ERR"
		}
		return decodeSeq(parts[1:])
	case "DEC":
		if len(parts) != 2 {
			return "DEC ERR"
		}
		raw, err := unhex(parts[1])
		if err != nil {
			return "DEC ERR"
		}
		return decodeBlock(raw)
	case "FRM":
		if len(parts) != 2 {
			return "FRM ERR"
		}
		raw, err := unhex(parts[1])
		if err != nil {
			return "FRM ERR"
		}
		return decodeFrame(raw)
	default:
		return "ERR " + strconv.Itoa(len(parts))
	}
}

func main() {
	in := bufio.NewScanner(os.Stdin)
	in.Buffer(make([]byte, 0, 1<<20), 1<<20)
	for in.Scan() {
		fmt.Println(handle(in.Text()))
	}
}
