#!/usr/bin/env python3

"""
Python SSE server with proper disconnection detection
This demonstrates that Python can handle SSE client disconnections properly
"""

import asyncio
import json
import time
import signal
import sys
from datetime import datetime, timezone
from http.server import HTTPServer, BaseHTTPRequestHandler
from urllib.parse import urlparse
import threading
from typing import Dict, Optional

PORT = 5008

# Store active SSE connections
clients: Dict[str, 'SSEClient'] = {}
client_counter = 0
clients_lock = threading.Lock()


class SSEClient:
    def __init__(self, client_id: str, request_handler):
        self.id = client_id
        self.request_handler = request_handler
        self.connected_at = time.time() * 1000  # milliseconds
        self.active = True
        self.ping_thread: Optional[threading.Thread] = None
        self.disconnected_event = threading.Event()


def generate_alphabetic_id(n: int) -> str:
    """Generate alphabetic IDs (A, B, C, ..., Z, AA, AB, ...)"""
    result = ""
    num = n
    while True:
        remainder = num % 26
        quotient = num // 26
        char = chr(65 + remainder)
        result = char + result
        if quotient == 0:
            break
        num = quotient - 1
    return result


def log_message(msg: str):
    """Log message with timestamp"""
    timestamp = datetime.now(timezone.utc).strftime(
        '%Y-%m-%dT%H:%M:%S.%fZ')[:-3] + 'Z'
    print(f"[{timestamp}] {msg}")


def cleanup_client(client_id: str):
    """Cleanup client connection"""
    with clients_lock:
        client = clients.pop(client_id, None)
        if client:
            client.active = False
            client.disconnected_event.set()  # Signal main thread immediately
            if client.ping_thread and client.ping_thread.is_alive():
                # Thread will stop naturally when active becomes False
                pass
            log_message(
                f"Client {client_id} removed. Remaining: {len(clients)}")


def ping_loop(client: SSEClient):
    """Ping loop for a client"""
    while client.active:
        time.sleep(1)
        if not client.active:
            break

        try:
            with clients_lock:
                total_clients = len(clients)

            timestamp = int(time.time() * 1000)
            ping_data = {
                "type": "ping",
                "timestamp": timestamp,
                "clientId": client.id,
                "totalClients": total_clients
            }

            message = f"data: {json.dumps(ping_data)}\n\n"
            client.request_handler.wfile.write(message.encode())
            client.request_handler.wfile.flush()
            log_message(f"Ping sent to {client.id}")

        except (BrokenPipeError, ConnectionResetError, OSError) as e:
            log_message(f"Connection error for {client.id}: {e}")
            cleanup_client(client.id)
            break
        except Exception as e:
            log_message(f"Unexpected error for {client.id}: {e}")
            cleanup_client(client.id)
            break


class SSEHandler(BaseHTTPRequestHandler):
    def log_message(self, format, *args):
        # Suppress default HTTP server logs
        pass

    def do_OPTIONS(self):
        """Handle OPTIONS requests for CORS"""
        self.send_response(200)
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', 'GET, OPTIONS')
        self.send_header('Access-Control-Allow-Headers', 'Content-Type')
        self.end_headers()

    def do_GET(self):
        """Handle GET requests"""
        parsed_path = urlparse(self.path)
        path = parsed_path.path

        if path == '/events':
            self.handle_events()
        elif path == '/status':
            self.handle_status()
        else:
            self.handle_404()

    def handle_events(self):
        """Handle SSE events endpoint"""
        global client_counter

        client_id = generate_alphabetic_id(client_counter)
        client_counter += 1

        log_message(f"Client {client_id} connecting...")

        # Set SSE headers
        self.send_response(200)
        self.send_header('Content-Type', 'text/event-stream')
        self.send_header('Cache-Control', 'no-cache')
        self.send_header('Connection', 'keep-alive')
        self.send_header('Access-Control-Allow-Origin', '*')
        self.end_headers()

        # Create client
        client = SSEClient(client_id, self)

        with clients_lock:
            clients[client_id] = client
            total_clients = len(clients)

        log_message(
            f"Client {client_id} connected. Total clients: {total_clients}")

        # Send initial connection message
        try:
            initial_data = {
                "type": "connected",
                "clientId": client_id,
                "message": "Python SSE connection established"
            }
            initial_message = f"data: {json.dumps(initial_data)}\n\n"
            self.wfile.write(initial_message.encode())
            self.wfile.flush()
        except Exception as e:
            log_message(f"Error sending initial message to {client_id}: {e}")
            cleanup_client(client_id)
            return

        # Start ping thread
        client.ping_thread = threading.Thread(
            target=ping_loop, args=(client,), daemon=True)
        client.ping_thread.start()

        # Monitor for disconnection
        try:
            # Wait for either disconnection event or periodic check
            while client.active:
                # Wait for disconnection event with timeout for periodic health check
                if client.disconnected_event.wait(0.5):
                    # Disconnection detected by ping thread
                    log_message(
                        f"Disconnection event received for {client_id}")
                    break

                # Periodic health check - try to detect disconnection directly
                try:
                    # Try to write a small comment that won't appear in the SSE stream
                    self.wfile.write(b": heartbeat\n")
                    self.wfile.flush()
                except (BrokenPipeError, ConnectionResetError, OSError):
                    log_message(f"Connection lost for {client_id}")
                    cleanup_client(client_id)
                    break
        except Exception as e:
            log_message(f"Connection monitoring error for {client_id}: {e}")
            cleanup_client(client_id)
        finally:
            # Ensure cleanup happens even if not already done
            if client_id in clients:
                cleanup_client(client_id)

    def handle_status(self):
        """Handle status endpoint"""
        self.send_response(200)
        self.send_header('Content-Type', 'application/json')
        self.send_header('Access-Control-Allow-Origin', '*')
        self.end_headers()

        with clients_lock:
            clients_info = [
                {
                    "id": client.id,
                    "connectedAt": client.connected_at
                }
                for client in clients.values()
            ]
            total_clients = len(clients)

        status = {
            "activeConnections": total_clients,
            "clients": clients_info,
            "implementation": "Python (proper disconnection detection)"
        }

        response = json.dumps(status, indent=2)
        self.wfile.write(response.encode())

    def handle_404(self):
        """Handle 404 responses"""
        self.send_response(404)
        self.send_header('Content-Type', 'text/plain')
        self.end_headers()

        message = "Not Found\n\nAvailable endpoints:\n- /events (SSE)\n- /status (JSON)"
        self.wfile.write(message.encode())


def shutdown_handler(signum, frame):
    """Handle shutdown gracefully"""
    log_message("\nShutting down...")

    # Close all client connections
    with clients_lock:
        for client_id, client in list(clients.items()):
            log_message(f"Closing connection {client_id}")
            try:
                client.active = False
            except Exception as e:
                log_message(f"Error closing {client_id}: {e}")
        clients.clear()

    log_message("Server closed")
    sys.exit(0)


def main():
    # Setup signal handlers
    signal.signal(signal.SIGINT, shutdown_handler)
    signal.signal(signal.SIGTERM, shutdown_handler)

    print()
    print(f"🐍 Python SSE Server Starting (Port {PORT})")
    print("✅ This implementation has proper disconnection detection:")
    print("   - Python HTTP server detects client disconnections through exceptions")
    print("   - Immediate disconnection detection through exception handling")
    print("   - Thread-safe client management with locks")
    print()
    print("Expected behavior: Immediate and reliable disconnection detection")
    print()
    print("Endpoints:")
    print(f"  - http://localhost:{PORT}/events (SSE stream)")
    print(f"  - http://localhost:{PORT}/status (Active connections)")

    try:
        server = HTTPServer(('localhost', PORT), SSEHandler)
        log_message("Server started successfully")
        server.serve_forever()
    except Exception as e:
        log_message(f"Server error: {e}")
        sys.exit(1)


if __name__ == '__main__':
    main()
