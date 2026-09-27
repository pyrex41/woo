"""Identify the live fixture for a conformance run; no external dependencies."""
import http.client
import os
import resource
import socket
import sys
import time


def require_live(pid):
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        raise SystemExit("Woo fixture exited before conformance completed")


def probe(port, nonce):
    connection = http.client.HTTPConnection("127.0.0.1", port, timeout=0.5)
    try:
        connection.request("GET", "/.woo-test-ready", headers={"Connection": "close"})
        response = connection.getresponse()
        return response.status == 200 and response.read(len(nonce) + 1) == nonce.encode("ascii")
    except (OSError, http.client.HTTPException):
        return False
    finally:
        connection.close()


def main():
    mode = sys.argv[1]
    if mode == "exec":
        # A noisy or failing fixture must not fill the diagnostic artifact
        # volume. Exec preserves the PID checked by the readiness probes.
        log_limit = 64 * 1024 * 1024
        resource.setrlimit(resource.RLIMIT_FSIZE, (log_limit, log_limit))
        os.execvp(sys.argv[2], sys.argv[2:])
    if mode == "port":
        with socket.socket() as listener:
            try:
                listener.bind(("127.0.0.1", int(sys.argv[2])))
            except OSError as error:
                raise SystemExit("Woo fixture port is unavailable: " + str(error))
            print(listener.getsockname()[1])
        return
    port, pid, nonce = int(sys.argv[2]), int(sys.argv[3]), sys.argv[4]
    deadline = time.monotonic() + (120 if mode == "wait" else 0)
    while True:
        require_live(pid)
        if probe(port, nonce):
            require_live(pid)
            return
        if time.monotonic() >= deadline:
            raise SystemExit("Woo fixture readiness identity did not match this run")
        time.sleep(0.05)


if __name__ == "__main__":
    main()
