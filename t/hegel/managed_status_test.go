package hegeltest

import (
	"fmt"
	"io"
	"net/http"
	"testing"
)

// This is a wire contract test, including unassigned final status codes.
func TestManagedFinalStatusRange(t *testing.T) {
	addr := managedFixture(t)
	for name, lane := range managedClients(t, addr) {
		t.Run(name, func(t *testing.T) {
			lane.client.CheckRedirect = func(_ *http.Request, _ []*http.Request) error {
				return http.ErrUseLastResponse
			}
			for code := 200; code <= 599; code++ {
				response, err := lane.client.Get(fmt.Sprintf("%s/status/%d", lane.base, code))
				if err != nil {
					t.Fatalf("status %d: %v", code, err)
				}
				body, err := io.ReadAll(io.LimitReader(response.Body, 1024))
				response.Body.Close()
				if err != nil {
					t.Fatalf("status %d body: %v", code, err)
				}
				if response.StatusCode != code || len(body) != 0 {
					t.Fatalf("status %d: received %d with %d body bytes", code,
						response.StatusCode, len(body))
				}
			}
		})
	}
}
