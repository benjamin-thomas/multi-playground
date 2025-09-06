/*
 * Rust SSE Server with Disconnection Detection
 *
 * This implementation demonstrates:
 * 1. Using Tokio's async runtime for connection management
 * 2. Axum web framework with SSE streaming support
 * 3. Custom stream wrapper for automatic client cleanup
 * 4. Drop trait for resource management when connections end
 *
 * PIN/UNPIN CRASH COURSE:
 * =======================
 *
 * WHY does Pin/Unpin exist? The problem is ASYNC FUNCTIONS.
 *
 * When you write:
 *   async fn foo() {
 *       let x = String::new();
 *       some_async_call(&x).await;
 *   }
 *
 * Rust transforms this into a state machine (simplified):
 *   struct FooFuture {
 *       x: String,                    // Local variable
 *       x_ref: Option<*const String>, // Points to x above!
 *       state: FooState,
 *   }
 *
 * THE PROBLEM: If FooFuture moves in memory, x_ref becomes invalid!
 * This would be undefined behavior - exactly what Rust prevents.
 *
 * THE SOLUTION: Pin<T>
 * - Pin<T> = "I promise this T will never move in memory"
 * - Once pinned, the value stays at that memory address forever
 * - This makes self-referential structs safe
 *
 * Two categories of types:
 * 1. Unpin types: Safe to move even when pinned (most types)
 * 2. !Unpin types: Must never move once pinned (async blocks, some futures)
 *
 * PRACTICAL RULES:
 * - Use Box::pin(async_block) for async blocks
 * - Add + Unpin bounds to avoid unsafe code in generic functions
 * - Most types implement Unpin automatically, so it's usually not restrictive
 */

use axum::{
    extract::State,
    http::StatusCode,
    response::{sse::Event, IntoResponse, Sse},
    routing::get,
    Json, Router,
};
use futures_util::stream::{self, Stream, StreamExt};
use serde::Serialize;
use std::{
    collections::HashMap,
    sync::{
        atomic::{AtomicU32, Ordering},
        Arc, Mutex,
    },
    time::{Duration, SystemTime, UNIX_EPOCH},
};
use tokio::time::interval;
use tokio_stream::wrappers::IntervalStream;
use tower_http::cors::CorsLayer;

const PORT: u16 = 5012;

// SSE Client data structure
#[derive(Debug, Clone, Serialize)]
pub struct SSEClient {
    pub id: String,
    pub connected_at: u64,
}

#[derive(Debug, Clone, Serialize)]
pub struct SSEMessage {
    pub r#type: String,
    #[serde(rename = "clientId")]
    pub client_id: String,
    pub message: Option<String>,
    pub timestamp: Option<u64>,
    #[serde(rename = "totalClients")]
    pub total_clients: Option<usize>,
}

#[derive(Debug, Clone, Serialize)]
pub struct StatusResponse {
    #[serde(rename = "activeConnections")]
    pub active_connections: usize,
    pub clients: Vec<SSEClient>,
    pub implementation: String,
}

pub type ClientMap = Arc<Mutex<HashMap<String, SSEClient>>>;
pub type ClientCounter = Arc<AtomicU32>;

// Generate alphabetic IDs (A, B, C, ..., Z, AA, AB, ...)
fn generate_alphabetic_id(mut n: u32) -> String {
    let mut result = String::new();
    loop {
        result.insert(0, (b'A' + (n % 26) as u8) as char);
        if n < 26 {
            break;
        }
        n = n / 26 - 1;
    }
    result
}

// Log message with timestamp
fn log_message(msg: &str) {
    let timestamp = chrono::Utc::now().format("%Y-%m-%dT%H:%M:%S.%3fZ");
    println!("[{}] {}", timestamp, msg);
}

// Stream wrapper that automatically cleans up when the connection ends
//
// WHY do we need the `+ Unpin` bound in our Stream implementation?
//
// Our poll_next function needs to call poll_next on the inner stream.
// To do that, we need Pin<&mut inner_stream>.
//
// There are two ways to create Pin<&mut T>:
// 1. Pin::new(&mut value) - SAFE, but only works if T: Unpin
// 2. Pin::new_unchecked(&mut value) - UNSAFE, works for any T
//
// We choose option 1 (safe) by requiring S: Unpin.
// This forces callers to use Box::pin for non-Unpin types (like async blocks).
//
// ALTERNATIVE: We could use unsafe Pin::new_unchecked, but that's... unsafe!
// Better to push the complexity to the caller (Box::pin) than hide it with unsafe.
struct CleanupStream<S> {
    stream: S,          // The actual SSE stream
    clients: ClientMap, // Client registry to clean up from
    client_id: String,  // Which client this stream represents
}

impl<S> CleanupStream<S> {
    fn new(stream: S, clients: ClientMap, client_id: String) -> Self {
        Self {
            stream,
            clients,
            client_id,
        }
    }
}

// Make our wrapper act like a stream by forwarding to the inner stream
impl<S> Stream for CleanupStream<S>
where
    S: Stream<Item = Result<Event, axum::Error>> + Unpin,
{
    type Item = Result<Event, axum::Error>;

    fn poll_next(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        // Forward the poll to our inner stream
        //
        // DEEP DIVE: What's happening here?
        // 1. `self` is Pin<&mut CleanupStream<S>>
        // 2. We want to call poll_next on self.stream
        // 3. poll_next requires Pin<&mut S>
        // 4. We get &mut S from Pin<&mut CleanupStream<S>>
        // 5. Pin::new(&mut S) creates Pin<&mut S> - SAFE because S: Unpin
        //
        // The Unpin bound means: "even if this CleanupStream is pinned,
        // it's safe to move the inner stream around in memory"
        std::pin::Pin::new(&mut self.stream).poll_next(cx)
    }
}

// The magic happens here! Drop is called when:
// - Client closes browser tab
// - Network connection breaks
// - Server shuts down
// - Any other reason the stream gets destroyed
impl<S> Drop for CleanupStream<S> {
    fn drop(&mut self) {
        // Remove this client from the active clients list
        let mut clients_guard = self.clients.lock().unwrap();
        if clients_guard.remove(&self.client_id).is_some() {
            log_message(&format!(
                "Client {} removed. Remaining: {}",
                self.client_id,
                clients_guard.len()
            ));
        }
    }
}

// Handle SSE events endpoint
async fn handle_events(
    State((clients, counter)): State<(ClientMap, ClientCounter)>,
) -> Sse<impl Stream<Item = Result<Event, axum::Error>>> {
    // Generate sequential client IDs (A, B, C, ...)
    let client_number = counter.fetch_add(1, Ordering::SeqCst);
    let client_id = generate_alphabetic_id(client_number);
    log_message(&format!("Client {} connecting...", client_id));

    let connected_at = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis() as u64;

    let client = SSEClient {
        id: client_id.clone(),
        connected_at,
    };

    // Add client to map
    {
        let mut clients_guard = clients.lock().unwrap();
        clients_guard.insert(client_id.clone(), client);
        log_message(&format!(
            "Client {} connected. Total clients: {}",
            client_id,
            clients_guard.len()
        ));
    }

    let clients_for_stream = clients.clone();
    let client_id_for_stream = client_id.clone();

    // Send initial connection message
    let initial_msg = SSEMessage {
        r#type: "connected".to_string(),
        client_id: client_id_for_stream.clone(),
        message: Some("Rust SSE connection established".to_string()),
        timestamp: None,
        total_clients: None,
    };

    // Create initial "connected" message
    //
    // WHY Box::pin? Let me explain Pin/Unpin from the ground up:
    //
    // PROBLEM: Async code creates "self-referential" structs under the hood.
    // Example: When you write `async { let x = 5; &x }`, Rust creates something like:
    //   struct AsyncBlock {
    //       x: i32,           // The variable
    //       x_ref: *mut i32,  // Points to x above!
    //   }
    // If this struct moves in memory, x_ref becomes invalid (dangling pointer)!
    //
    // SOLUTION: Pin<T> = "I promise this won't move in memory"
    // - Pin::new(value) - only works if T: Unpin (most types)
    // - Box::pin(value) - allocates on heap, heap address never changes
    //
    // UNPIN TRAIT: "It's safe to move me even when pinned"
    // - Most types implement Unpin automatically (i32, String, Vec, etc.)
    // - Async blocks do NOT implement Unpin (they have self-references)
    // - That's why we need Box::pin for our async block
    //
    // UNDER THE HOOD: Box::pin allocates the async block on the heap.
    // Heap addresses don't change, so self-references stay valid.
    let initial_event = Box::pin(stream::once(async move {
        Ok(Event::default().data(serde_json::to_string(&initial_msg).unwrap()))
    }));

    // Create ping stream that sends periodic updates
    //
    // WHY is this Unpin but async blocks aren't?
    // IntervalStream doesn't contain self-references - it just holds:
    // - A timer
    // - Some configuration
    // None of these point to each other, so it's safe to move around.
    // The `.map()` just transforms values, doesn't create self-references either.
    let ping_stream = IntervalStream::new(interval(Duration::from_secs(1))).map(move |_| {
        let total_clients = {
            let clients_guard = clients_for_stream.lock().unwrap();
            clients_guard.len()
        };

        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_millis() as u64;

        let ping_msg = SSEMessage {
            r#type: "ping".to_string(),
            client_id: client_id_for_stream.clone(),
            message: None,
            timestamp: Some(timestamp),
            total_clients: Some(total_clients),
        };

        log_message(&format!("Ping sent to {}", client_id_for_stream));

        Ok(Event::default().data(serde_json::to_string(&ping_msg).unwrap()))
    });

    // Chain the streams together: first the initial message, then continuous pings
    let combined_stream = initial_event.chain(ping_stream);
    let cleanup_stream = CleanupStream::new(combined_stream, clients, client_id);

    Sse::new(cleanup_stream).keep_alive(
        axum::response::sse::KeepAlive::new()
            .interval(Duration::from_secs(15))
            .text("keep-alive-text"),
    )
}

// Handle status endpoint
async fn handle_status(
    State((clients, _counter)): State<(ClientMap, ClientCounter)>,
) -> Json<StatusResponse> {
    let clients_guard = clients.lock().unwrap();
    let client_list: Vec<SSEClient> = clients_guard.values().cloned().collect();

    Json(StatusResponse {
        active_connections: client_list.len(),
        clients: client_list,
        implementation: "Rust (proper disconnection detection)".to_string(),
    })
}

// Handle 404 - Clean and simple
async fn handle_404() -> impl IntoResponse {
    let message = "Not Found\n\nAvailable endpoints:\n- /events (SSE)\n- /status (JSON)";
    (StatusCode::NOT_FOUND, message)
}

#[tokio::main]
async fn main() {
    // Initialize client storage and counter
    let clients: ClientMap = Arc::new(Mutex::new(HashMap::new()));
    let counter: ClientCounter = Arc::new(AtomicU32::new(0));

    // Removed unnecessary test function call

    // Create router
    let app = Router::new()
        .route("/events", get(handle_events))
        .route("/status", get(handle_status))
        .fallback(handle_404)
        .layer(CorsLayer::permissive())
        .with_state((clients, counter));

    println!();
    println!("🦀 Rust SSE Server Starting (Port {})", PORT);
    println!("✅ This implementation features:");
    println!("   - Tokio async runtime for connection management");
    println!("   - Axum web framework with SSE support");
    println!("   - Automatic client cleanup via Drop trait");
    println!("   - Thread-safe client tracking with atomic counters");
    println!();
    println!("Expected behavior: Fast disconnection detection");
    println!();
    println!("Endpoints:");
    println!("  - http://localhost:{}/events (SSE stream)", PORT);
    println!("  - http://localhost:{}/status (Active connections)", PORT);

    // Start server
    let listener = tokio::net::TcpListener::bind(format!("127.0.0.1:{}", PORT))
        .await
        .unwrap();

    log_message(&format!("Server started successfully on port {}", PORT));

    axum::serve(listener, app).await.unwrap();
}
