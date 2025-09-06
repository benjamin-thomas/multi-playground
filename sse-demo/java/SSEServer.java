import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Java SSE server with proper disconnection detection
 * This demonstrates that Java can handle SSE client disconnections properly
 */
public class SSEServer {
    private static final int PORT = 5004;
    private static final ConcurrentHashMap<String, SSEClient> clients = new ConcurrentHashMap<>();
    private static final AtomicInteger clientCounter = new AtomicInteger(0);
    private static final DateTimeFormatter timeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
            .withZone(ZoneId.of("UTC"));

    static class SSEClient {
        final String id;
        final OutputStream outputStream;
        final Thread pingThread;
        final long connectedAt;
        volatile boolean active;

        SSEClient(String id, OutputStream outputStream, Thread pingThread) {
            this.id = id;
            this.outputStream = outputStream;
            this.pingThread = pingThread;
            this.connectedAt = System.currentTimeMillis();
            this.active = true;
        }
    }

    // Generate alphabetic IDs (A, B, C, ..., Z, AA, AB, ...)
    private static String generateAlphabeticId(int n) {
        StringBuilder result = new StringBuilder();
        int num = n;
        do {
            result.insert(0, (char) (65 + (num % 26)));
            num = num / 26 - 1;
        } while (num >= 0);
        return result.toString();
    }

    private static void logMessage(String message) {
        System.out.println("[" + timeFormatter.format(Instant.now()) + "] " + message);
    }

    // Handle SSE events endpoint
    static class EventsHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            if ("OPTIONS".equals(exchange.getRequestMethod())) {
                handleOptions(exchange);
                return;
            }

            if (!"GET".equals(exchange.getRequestMethod())) {
                exchange.sendResponseHeaders(405, 0);
                exchange.close();
                return;
            }

            String clientId = generateAlphabeticId(clientCounter.getAndIncrement());
            logMessage("Client " + clientId + " connecting...");

            // Set SSE headers
            exchange.getResponseHeaders().set("Content-Type", "text/event-stream");
            exchange.getResponseHeaders().set("Cache-Control", "no-cache");
            exchange.getResponseHeaders().set("Connection", "keep-alive");
            exchange.getResponseHeaders().set("Access-Control-Allow-Origin", "*");
            exchange.sendResponseHeaders(200, 0);

            OutputStream outputStream = exchange.getResponseBody();

            // Send initial connection message
            String initialMessage = String.format(
                "data: {\"type\":\"connected\",\"clientId\":\"%s\",\"message\":\"Java SSE connection established\"}\n\n",
                clientId
            );

            try {
                outputStream.write(initialMessage.getBytes());
                outputStream.flush();
            } catch (IOException e) {
                logMessage("Error sending initial message to " + clientId + ": " + e.getMessage());
                exchange.close();
                return;
            }

            // Start ping thread
            Thread pingThread = new Thread(() -> {
                try {
                    while (!Thread.currentThread().isInterrupted()) {
                        Thread.sleep(1000);

                        SSEClient client = clients.get(clientId);
                        if (client == null || !client.active) {
                            break;
                        }

                        String pingMessage = String.format(
                            "data: {\"type\":\"ping\",\"timestamp\":%d,\"clientId\":\"%s\",\"totalClients\":%d}\n\n",
                            System.currentTimeMillis(),
                            clientId,
                            clients.size()
                        );

                        try {
                            outputStream.write(pingMessage.getBytes());
                            outputStream.flush();
                            logMessage("Ping sent to " + clientId);
                        } catch (IOException e) {
                            logMessage("Error sending ping to " + clientId + ": " + e.getMessage());
                            cleanup(clientId);
                            break;
                        }
                    }
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    logMessage("Ping thread interrupted for " + clientId);
                }
            });

            // Store client
            SSEClient client = new SSEClient(clientId, outputStream, pingThread);
            clients.put(clientId, client);
            logMessage("Client " + clientId + " connected. Total clients: " + clients.size());

            pingThread.start();

            // Monitor connection - Java HttpServer will detect disconnection
            // when the client closes the connection
            try {
                // Wait for ping thread to complete (indicates disconnection)
                pingThread.join();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                logMessage("Connection monitoring interrupted for " + clientId);
            } finally {
                cleanup(clientId);
                exchange.close();
            }
        }
    }

    // Handle status endpoint
    static class StatusHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            if ("OPTIONS".equals(exchange.getRequestMethod())) {
                handleOptions(exchange);
                return;
            }

            if (!"GET".equals(exchange.getRequestMethod())) {
                exchange.sendResponseHeaders(405, 0);
                exchange.close();
                return;
            }

            exchange.getResponseHeaders().set("Content-Type", "application/json");
            exchange.getResponseHeaders().set("Access-Control-Allow-Origin", "*");

            StringBuilder clientsJson = new StringBuilder();
            int index = 0;
            for (SSEClient client : clients.values()) {
                if (index > 0) clientsJson.append(",\n    ");
                clientsJson.append(String.format(
                    "{\"id\": \"%s\", \"connectedAt\": %d}",
                    client.id, client.connectedAt
                ));
                index++;
            }

            String status = String.format(
                "{\n" +
                "  \"activeConnections\": %d,\n" +
                "  \"clients\": [\n" +
                "    %s\n" +
                "  ],\n" +
                "  \"implementation\": \"Java (proper disconnection detection)\"\n" +
                "}",
                clients.size(),
                clientsJson.toString()
            );

            byte[] response = status.getBytes();
            exchange.sendResponseHeaders(200, response.length);
            OutputStream os = exchange.getResponseBody();
            os.write(response);
            os.close();
        }
    }

    private static void handleOptions(HttpExchange exchange) throws IOException {
        exchange.getResponseHeaders().set("Access-Control-Allow-Origin", "*");
        exchange.getResponseHeaders().set("Access-Control-Allow-Methods", "GET, OPTIONS");
        exchange.getResponseHeaders().set("Access-Control-Allow-Headers", "Content-Type");
        exchange.sendResponseHeaders(200, 0);
        exchange.close();
    }

    private static void cleanup(String clientId) {
        SSEClient client = clients.remove(clientId);
        if (client != null) {
            client.active = false;
            client.pingThread.interrupt();
            logMessage("Client " + clientId + " removed. Remaining: " + clients.size());
        }
    }

    public static void main(String[] args) throws IOException {
        HttpServer server = HttpServer.create(new InetSocketAddress(PORT), 0);
        server.createContext("/events", new EventsHandler());
        server.createContext("/status", new StatusHandler());
        server.setExecutor(Executors.newCachedThreadPool());

        // Shutdown hook
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            logMessage("\nShutting down...");

            // Close all client connections
            for (SSEClient client : clients.values()) {
                logMessage("Closing connection " + client.id);
                try {
                    client.active = false;
                    client.pingThread.interrupt();
                    client.outputStream.close();
                } catch (IOException e) {
                    logMessage("Error closing " + client.id + ": " + e.getMessage());
                }
            }
            clients.clear();

            server.stop(2);
            logMessage("Server closed");
        }));

        logMessage("");
        logMessage("🟠 Java SSE Server Starting (Port " + PORT + ")");
        logMessage("✅ This implementation has proper disconnection detection:");
        logMessage("   - Java HttpServer detects client disconnections through IOException");
        logMessage("   - Immediate disconnection detection through exception handling");
        logMessage("   - Thread-safe client management with ConcurrentHashMap");
        logMessage("");
        logMessage("Expected behavior: Immediate and reliable disconnection detection");
        logMessage("");
        logMessage("Endpoints:");
        logMessage("  - http://localhost:" + PORT + "/events (SSE stream)");
        logMessage("  - http://localhost:" + PORT + "/status (Active connections)");

        server.start();
    }
}