// Differential oracle: Go's HTTP/2 stack (golang.org/x/net/http2).
// One request per stdin line. Fields are hex.
//
//	ENC <namehex> <valuehex> ...  -> ENC <blockhex> | ENC ERR
//	DEC <blockhex>                 -> DEC <namehex> <valuehex> ... | DEC ERR
//	FRM <framehex>                 -> FRM OK <type> <flags> <sid> <length> | FRM SHORT | FRM ERR
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

func encodeFields(fields []hpack.HeaderField) string {
	var buf bytes.Buffer
	enc := hpack.NewEncoder(&buf)
	for _, f := range fields {
		if err := enc.WriteField(f); err != nil {
			return "ENC ERR"
		}
	}
	return "ENC " + hex.EncodeToString(buf.Bytes())
}

func decodeBlock(raw []byte) string {
	dec := hpack.NewDecoder(4096, nil)
	hs, err := dec.DecodeFull(raw)
	if err != nil {
		return "DEC ERR"
	}
	var b strings.Builder
	b.WriteString("DEC")
	for _, h := range hs {
		fmt.Fprintf(&b, " %s %s", hex.EncodeToString([]byte(h.Name)), hex.EncodeToString([]byte(h.Value)))
	}
	return b.String()
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
	return fmt.Sprintf("FRM OK %d %d %d %d", h.Type, h.Flags, h.StreamID, h.Length)
}

func handle(line string) string {
	parts := strings.Fields(line)
	if len(parts) == 0 {
		return "ERR"
	}
	switch parts[0] {
	case "ENC":
		if len(parts)%2 != 1 {
			return "ENC ERR"
		}
		var fields []hpack.HeaderField
		for i := 1; i < len(parts); i += 2 {
			name, err1 := unhex(parts[i])
			value, err2 := unhex(parts[i+1])
			if err1 != nil || err2 != nil {
				return "ENC ERR"
			}
			fields = append(fields, hpack.HeaderField{Name: string(name), Value: string(value)})
		}
		return encodeFields(fields)
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
