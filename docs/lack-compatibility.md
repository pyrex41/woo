# Managed Lack compatibility

`woo-lack-compat` provides an opt-in Clack adapter and explicit Lack middleware
helpers. Qualification requires fresh receipts from the exact checkout.
The profile remains experimental;
[production readiness](production-readiness.md) is **UNKNOWN** pending complete
protocol coverage and deployment validation.

## Start and stop

Register this checkout with ASDF as shown in the
[installation guide](../README.md#install-and-run). Loading the managed system
requires the native zstd library in addition to Woo's native dependencies.

```lisp
(ql:quickload :woo-lack-compat)

(defun my-api (env)
  (declare (ignore env))
  '(200 (:content-type "application/json") ("{\"ok\":true}")))

(defun my-app (env)
  (declare (ignore env))
  '(200 (:content-type "text/plain") ("Hello, World")))

(defparameter *server*
  (woo.compat:clackup
    (lack:builder
      :woo-backtrace
      :woo-accesslog
      :woo-deflater
      :woo-session
      (:woo-mount "/api" #'my-api)
      #'my-app)
    :address "127.0.0.1"
    :port 5000))

(woo.compat:server-state *server*)
(clack:stop *server*)
```

The returned value is an ordinary Clack handle for `:server :woo-managed`.
The adapter owns its network thread and application workers. Outer Clack
threading must be disabled; `woo.compat:clackup` sets `:use-thread nil`,
`:use-default-middlewares nil`, and defaults `:debug` to nil. Use explicit
middleware. The managed profile uses one network loop and requires
`:worker-num nil` (the default); set `:application-workers` instead. Process signal handling
belongs to the host application. Use `clack:stop` from its shutdown path.

Applications and delayed producers execute on a bounded worker pool. HTTP/1
requests on a connection are answered in order. HTTP/2 streams execute
independently. Socket, TLS, parser, HPACK and response-queue operations belong
to the network loop. Ordinary pathname bodies are read in bounded slices on
application workers. Request bodies use bounded memory without disk spooling.

Legacy `woo:run` and `:server :woo` keep their scheduling defaults. The shared
socket flush fix preserves writes queued by completion callbacks, and the
shared Host parser handles bracketed IPv6.

## Limits and lifecycle

| Option | Default |
| --- | ---: |
| `:application-workers` | 4 |
| `:max-pending-requests` | 64 waiting jobs |
| `:max-connection-requests` | 16 admitted application requests |
| `:max-request-body-bytes` | 64 MiB |
| `:max-request-storage-bytes` | 256 MiB per server |
| `:max-response-queue-bytes` | 8 MiB per response |
| `:max-connection-queue-bytes` | 64 MiB |
| `:max-server-queue-bytes` | 256 MiB |
| `:startup-timeout` | 10 seconds |
| `:drain-timeout` | 10 seconds |
| `:cleanup-timeout` | 5 seconds |

Admission occurs before copying queued body/output data. These limits account
for admitted octets and copied unsent output, including socket buffers; they
are not a bound on total Lisp heap usage or application-owned allocations.
Ordinary response pumps wait for output progress. Streaming writes that exceed
the budget fail and cancel the response. HTTP/1 framing errors close the
connection; HTTP/2 response errors reset that stream.

Stop closes admission, sends HTTP/2 GOAWAY, and drains to the deadline before
closing remaining sockets. An application that does not return is retained and
reported with `woo.compat:shutdown-timeout`; the condition exposes live threads
through `shutdown-timeout-workers`. Release or finish those workers and retry
`clack:stop`. Arbitrary application threads are not destroyed. Application-owned
DB pools and stores remain the application's responsibility.

## Application contract

Ordinary responses `(status headers body)`, delayed responders, streaming
writers, Unicode/binary bodies, pathname bodies, HEAD, bodyless statuses and
repeated Set-Cookie fields are supported. Responders accept headers once;
writers return NIL after close or cancellation. Declared Content-Length must
match emitted bytes. Header names and values are validated before sending.

Managed TLS listeners advertise `h2` and `http/1.1` automatically. Their
environment uses `:url-scheme "https"` and default Host port 443; an explicit
Host port is preserved. Legacy HTTP/1 handling through `woo:run` keeps its
historical TLS environment values (`"http"` and default Host port 80).

Application responses use final status codes 200–599. Interim 1xx responses
are not exposed by this adapter; websocket-driver performs its HTTP/1 upgrade
through the raw socket API.

The environment contains `:woo.request-cancelled-p`, a function applications can
poll before starting additional work or committing application side effects.
Cancellation does not interrupt application code. A session commit already in
progress cannot be rolled back by a socket disconnect.

`:clack.io` is an opaque connection proxy. Generic `clack.socket` operations run
on the owning loop. HTTP/2 raw socket takeover is rejected. For HTTP/1
websocket-driver, perform state-changing driver operations through:

```lisp
(woo.compat:call-on-connection (getf env :clack.io)
  (lambda () (websocket-driver:start-connection websocket)))
```

The same rule applies to driver sends initiated by external threads. Message
callbacks already run on the owner loop. Keep those callbacks short.

## Explicit middleware

| Identifier | Behavior |
| --- | --- |
| `:woo-mount` | Clones the environment and updates SCRIPT-NAME/PATH-INFO |
| `:woo-deflater` | Incremental gzip/zstd for ordinary, delayed and streamed bodies; q-value negotiation, Vary and length cleanup |
| `:woo-session` | Uses Lack store/state APIs; serializes same-SID work and shared store connections; finalizes before responding; suppresses cancelled late commits |
| `:woo-accesslog` | Completion metadata; omits paths, queries, headers and bodies; queues the logger on workers |
| `:woo-backtrace` | Catches synchronous and delayed producer errors; reports error types without printing conditions or request secrets |

The default session store is in-memory and its state uses cookies. Supply
`:store` and `:state` to `:woo-session` for other Lack store/state objects.
SQLite and Redis stores and `:dbpool` require their corresponding Lack/DBI
systems to be loaded. `:lock-timeout` defaults to 10 seconds; a request waiting
too long for a session lease receives 503.

Session locks are process-local and shared across helper instances using the
same store or Redis connection. Session values in lists, hashes and arrays are
copied recursively; hash key identity is preserved. Other application objects
retain their identity, so their side effects remain application-owned.
Separate application processes need a store or
application protocol that provides atomic updates. Missing delayed responders
release their session lease on cancellation. The fixed stripe table may
serialize unrelated SIDs. Malformed serialized sessions are read with reader
evaluation disabled and dependency warnings are suppressed to avoid payload
logging.

The access logger shares the bounded application queue. Saturated/shutting-down
queues drop completion events and increment `:dropped-completions` in the
resource snapshot. Accepted body-byte counts do not establish delivery to the
remote application. An arbitrary application's later background-thread errors
must be handled by that application.

Use ordinary upstream `:auth-basic`, `:csrf`, `:static`, and `:dbpool` explicitly.
For CSRF, obtain the token inside the wrapped application's dynamic scope.
Compression drops ETag, digest and byte-range metadata for the original bytes.
Compression helpers require Salza2 and cl-zstd.

## Required validation

Run from the repository root. `nix develop` provides SBCL and the native
libraries, Redis, SQLite, zstd, Go, Python and resource-sampling tools. It does
not install Quicklisp. For a fresh, private Quicklisp installation inside that
shell:

```sh
nix develop
set -eu
woo_ql_dir=$(mktemp -d "${TMPDIR:-/tmp}/woo-quicklisp.XXXXXXXX")
curl --fail --max-time 60 --location https://beta.quicklisp.org/quicklisp.lisp \
  -o "$woo_ql_dir/bootstrap.lisp"
python3 -c 'import hashlib, sys; assert hashlib.sha256(open(sys.argv[1], "rb").read()).hexdigest() == "4a7a5c2aebe0716417047854267397e24a44d0cce096127411e9ce9ccfeb2c17"' \
  "$woo_ql_dir/bootstrap.lisp"
sbcl --non-interactive --load "$woo_ql_dir/bootstrap.lisp" \
  --eval "(quicklisp-quickstart:install :path \"$woo_ql_dir/quicklisp/\")"
export WOO_QUICKLISP_SETUP="$woo_ql_dir/quicklisp/setup.lisp"
sbcl --non-interactive --load "$WOO_QUICKLISP_SETUP" \
  --eval '(ql-dist:install-dist "http://beta.quicklisp.org/dist/quicklisp/2026-01-01/distinfo.txt" :prompt nil :replace t)'

python3 t/compat/check.py --dependencies /tmp/woo-compat-deps
python3 t/compat/check.py --verify-receipt .artifacts/lack/receipt.json
```

Alternatively, use an existing Quicklisp installation with the frozen
`2026-01-01` distribution. The default setup path is `~/quicklisp/setup.lisp`;
`WOO_QUICKLISP_SETUP` overrides it. Distribution selection changes that
installation, so use the private setup above to keep a different distribution.

The runner verifies immutable Clack/Lack/websocket-driver/Hunchentoot archive
pins from [dependencies.json](../t/compat/dependencies.json), downloads them to
the dependency directory, and registers those sources without patching the
installed upstream systems. It owns a private loopback Redis with persistence
disabled, generates test certificates, enforces time/log/descriptor budgets,
and writes source-bound
receipts. SQLite uses private fixtures. No missing service or dependency is
silently skipped. `--soak-seconds 30` is a diagnostic run and cannot produce a
qualification PASS. Source changes require fresh receipts.

Required gates:

- In-process contracts and Hunchentoot reference behavior.
- HTTP/1, HTTPS, h2c and HTTP/2 TLS: ordinary/delayed bodies, Unicode/binary,
  HEAD/bodyless statuses, repeated headers, mount, compression, auth/CSRF,
  static/pathname responses, sessions, SQLite pooling/stores and Redis stores.
- Response ordering, duplicate/late writes, request cancellation, admission
  limits, startup failure, active HTTP/2 drain and an uncooperative worker.
- Expired dispatch, completion registration races, compressed close refusal,
  errors before/after headers and stale/incomplete receipt rejection.
- 100 start/request/stop cycles, resource counters and descriptor baseline.
- HTTP/1 websocket-driver echo and a 30-minute transport soak with resource
  baseline checks, native descriptor samples and a 256 MiB RSS growth budget.
- Both Linux and macOS managed-profile jobs, plus the existing Lisp, Hegel and
  conformance-launcher CI gates.

`.github/workflows/lack-compatibility.yml` runs the managed gates on Linux and
macOS with a locked Nix environment and preserves receipts/logs on failure.
A receipt qualifies only its recorded Git HEAD, runtime/test source digest and
dependency pins. The verifier rejects a receipt at another HEAD, including a
later documentation commit. A local subset or diagnostic soak cannot qualify
a new version or a production deployment.

## Qualification receipts

A result applies only to its recorded HEAD, source digest, dependency pins,
platform and workload. Verify a complete 1800-second receipt with the command
above before calling the managed profile qualified. A short diagnostic,
load-only check, prior fork run or green run for a different HEAD does not qualify
this checkout. Production readiness remains UNKNOWN.
