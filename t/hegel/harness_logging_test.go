package hegeltest

import (
	"bytes"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"testing"
	"time"
)

// Exercise the real child-output path twice against one artifact, rather than
// testing the writer's counter in isolation. Each child emits more than the
// whole artifact budget before reporting readiness.
func TestFixtureDiagnosticLogBudget(t *testing.T) {
	t.Setenv("WOO_FIXTURE_LOG_BUDGET_CHILD", "1")
	path := filepath.Join(t.TempDir(), "memory.log")
	t.Setenv("WOO_LEGACY_MEMORY_LOG", path)
	binary, err := os.Executable()
	if err != nil {
		t.Fatal(err)
	}
	for attempt := 0; attempt < 2; attempt++ {
		_, err := startFixture(t, fixtureSpec{
			name:       "diagnostic log budget child",
			command:    []string{binary, "-test.run=^TestFixtureLogBudgetChild$"},
			readyPath:  "/ready",
			startupTTL: 5 * time.Second,
		}, "")
		if err != nil {
			t.Fatal(err)
		}
		info, err := os.Stat(path)
		if err != nil {
			t.Fatal(err)
		}
		if info.Size() != 256*1024 {
			t.Fatalf("attempt %d: shared diagnostic artifact has %d bytes, want 256 KiB", attempt, info.Size())
		}
	}
}

func TestFixtureLogBudgetChild(t *testing.T) {
	if os.Getenv("WOO_FIXTURE_LOG_BUDGET_CHILD") != "1" {
		t.Skip("subprocess fixture only")
	}
	if _, err := os.Stdout.Write(bytes.Repeat([]byte("x"), 512*1024)); err != nil {
		t.Fatal(err)
	}
	mux := http.NewServeMux()
	mux.HandleFunc("/ready", func(w http.ResponseWriter, _ *http.Request) {
		fmt.Fprint(w, os.Getenv("WOO_HEGEL_READY_NONCE"))
	})
	server := &http.Server{
		Addr:              "127.0.0.1:" + os.Getenv("WOO_HEGEL_PORT"),
		Handler:           mux,
		ReadHeaderTimeout: time.Second,
		WriteTimeout:      time.Second,
	}
	t.Fatal(server.ListenAndServe())
}
