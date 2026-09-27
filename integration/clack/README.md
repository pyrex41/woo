# Clack threaded shutdown integration

Woo's `stop-gracefully` asks its owning event loop to exit, closes accepted sockets, and lets the server thread finish cleanup. It does **not** drain active requests. The installed Clack threaded `stop` still destroys its acceptor thread, bypassing that cleanup.

[`threaded-stop.patch`](threaded-stop.patch) targets `src/handler.lisp` in the 2025-06-22 Quicklisp Clack release. It makes Clack call a handler's `STOP-GRACEFULLY` hook, wait for the thread, and join it. For Woo, the wait is bounded at 15 seconds, longer than Woo's 10-second worker-stop bound. Handlers without the hook retain Clack's existing fallback.

Apply it to the **active Clack source before loading Clack**:

~~~sh
cd /path/to/active/clack
patch --dry-run -p1 < /path/to/woo/integration/clack/threaded-stop.patch
patch -p1 < /path/to/woo/integration/clack/threaded-stop.patch
~~~

Verify that the application loaded the patched source and repeat the bounded start/stop descriptor test in that installation. A local smoke of a patched Clack copy completed 12 cycles with stable descriptors; the patch is **not installed upstream or in the default Quicklisp Clack**. Until it is installed and verified, use `woo:stop-gracefully` and join the `woo:run` thread directly for managed shutdown.
