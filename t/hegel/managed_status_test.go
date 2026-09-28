package hegeltest

import (
	"fmt"
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
				response, body := managedRequest(t, lane.client, "GET",
					fmt.Sprintf("%s/status/%d", lane.base, code), nil, nil)
				if response.StatusCode != code || len(body) != 0 {
					t.Fatalf("status %d: received %d with %d body bytes", code,
						response.StatusCode, len(body))
				}
			}
		})
	}
}
