## Gracefully stop threaded handlers before destroying threads

Clack's threaded handler path currently destroys the acceptor thread during `clack:stop`, which can bypass a handler's own shutdown hook and leave server resources in an incomplete state.

This change:

- imports Bordeaux Threads `join-thread`;
- detects and calls a handler package's optional `STOP-GRACEFULLY` hook;
- waits up to 15 seconds for the threaded acceptor to finish, then joins it;
- preserves the existing destroy-thread fallback for handlers without the hook or handlers that do not stop within the bound;
- adds focused threaded-handler tests for the hook and fallback paths.

### Test

After importing the bundle into a Clack clone and selecting `codex/threaded-stop`, run from that clone. Set `QUICKLISP_SETUP` to your frozen Quicklisp setup path; requires SBCL, Bordeaux Threads, Rove and the Clack test dependencies.

```sh
export QUICKLISP_SETUP="$HOME/quicklisp/setup.lisp"
sbcl --non-interactive \
  --load "$QUICKLISP_SETUP" \
  --eval '(push (truename "./") asdf:*central-registry*)' \
  --eval '(ql:quickload :clack-test :silent t)' \
  --eval '(unless (rove:run-suite :clack-test.handler-stop) (error "handler stop failed"))' \
  --quit
```

Result: 2 focused tests passed, plus 12 live Woo start/request/stop cycles on randomized ports. The hook test checks exactly one invocation and natural thread unwinding.

Base: `9435762a8fc139edde8c682502a037332b0655f8`. Head: `3fbf148681d559a5aea62f1b184516441fc336f6`. Live Woo runtime: `4d005a686a4a5287acd77050e927cc42a6f07665`. Logs and the Git bundle are attached beside this description. These are local results; hosted CI and upstream publication are pending.
