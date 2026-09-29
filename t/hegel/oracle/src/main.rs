use axum::{
    body::Bytes,
    extract::{
        ws::{Message, WebSocket, WebSocketUpgrade},
        Path, Request,
    },
    http::StatusCode,
    response::{IntoResponse, Response},
    routing::any,
    Router,
};
use std::{env, net::SocketAddr, process, time::Duration};
use tokio::net::TcpListener;

async fn echo(request: Request) -> Response {
    let path = request.uri().path().as_bytes().to_vec();
    (StatusCode::OK, [("content-type", "text/plain")], path).into_response()
}

async fn body(bytes: Bytes) -> Response {
    (
        StatusCode::OK,
        [("content-type", "application/octet-stream")],
        bytes,
    )
        .into_response()
}

async fn status(Path(code): Path<u16>) -> Response {
    let status = StatusCode::from_u16(code).unwrap_or(StatusCode::BAD_REQUEST);
    (status, [("content-type", "text/plain")], code.to_string()).into_response()
}

async fn static_fixture() -> Response {
    (StatusCode::OK, [("content-type", "text/plain")], "woo reference fixture\n").into_response()
}

async fn websocket(ws: WebSocketUpgrade) -> impl IntoResponse {
    ws.on_upgrade(echo_socket)
}

async fn ready() -> Response {
    match env::var("WOO_HEGEL_READY_NONCE") {
        Ok(nonce) => (StatusCode::OK, nonce).into_response(),
        Err(_) => StatusCode::NOT_FOUND.into_response(),
    }
}

async fn echo_socket(mut socket: WebSocket) {
    while let Some(Ok(message)) = socket.recv().await {
        // Woo's fixture deliberately echoes binary messages only.
        if let Message::Binary(payload) = message {
            if socket.send(Message::Binary(payload)).await.is_err() {
                return;
            }
        }
    }
}

fn app() -> Router {
    Router::new()
        .route("/.woo-test-ready", any(ready))
        .route("/body", any(body))
        .route("/upload", any(body))
        .route("/status/{code}", any(status))
        .route("/static/fixture.txt", any(static_fixture))
        .route("/ws", any(websocket))
        .route("/echo/{*path}", any(echo))
        .fallback(|| async {
            (
                StatusCode::NOT_FOUND,
                [("content-type", "text/plain")],
                "not found",
            )
        })
}

fn port() -> u16 {
    let environment_port = env::var("WOO_HEGEL_PORT").ok();
    let mut args = env::args().skip(1);
    while let Some(arg) = args.next() {
        if arg == "--port" {
            return args
                .next()
                .and_then(|value| value.parse().ok())
                .unwrap_or_else(|| {
                    eprintln!("--port requires a numeric value");
                    process::exit(2)
                });
        }
    }
    environment_port
        .as_deref()
        .and_then(|value| value.parse().ok())
        .unwrap_or(0)
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let listener = TcpListener::bind(SocketAddr::from(([127, 0, 0, 1], port()))).await?;
    let address = listener.local_addr()?;
    println!("READY {address}");
    // Make the readiness line observable when stdout is piped by the harness.
    use std::io::Write;
    std::io::stdout().flush()?;

    axum::serve(listener, app())
        .with_graceful_shutdown(async {
            let _ = tokio::signal::ctrl_c().await;
            tokio::time::sleep(Duration::from_millis(10)).await;
        })
        .await?;
    Ok(())
}
