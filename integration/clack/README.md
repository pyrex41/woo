# Graceful stop for Clack's threaded Woo handler

`threaded-stop.patch` targets Clack's `src/handler.lisp` from the 2025-06-22
Quicklisp release. It lets Clack ask a handler package for `STOP-GRACEFULLY`
when its acceptor is a thread. For Woo, that requests event-loop shutdown,
waits up to fifteen seconds for the thread to exit, and joins it. This allows
Woo's worker-cluster stop to use its ten-second bound before Clack's fallback.
Existing `destroy-thread` behavior remains a bounded fallback for handlers without the
hook or for a Woo shutdown that does not finish.

Apply this to the Clack source used by the application before loading Clack:

```sh
cd /path/to/clack
patch -p1 < /path/to/woo/integration/clack/threaded-stop.patch
```

The patch is an integration proposal, not a change to the installed Clack
package. Check that the active Clack source and version match, apply the patch
to that source, and verify it is loaded before relying on `clack:clackdown` for
graceful threaded Woo shutdown. Without it, Clack's threaded stop still calls
`destroy-thread`. The Woo-side `woo:stop-gracefully` API is also usable directly
with the thread running `woo:run`.
