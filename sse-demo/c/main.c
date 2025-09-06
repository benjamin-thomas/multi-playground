/*
 * C SSE Server with Mongoose
 *
 * This implementation demonstrates:
 * 1. Event-driven HTTP server using Mongoose library
 * 2. Manual memory management for client tracking
 * 3. Direct system-level connection handling
 * 4. Immediate disconnection detection via TCP events
 *
 * C ADVANTAGES FOR NETWORKING:
 * =============================
 *
 * MANUAL MEMORY MANAGEMENT:
 * - Complete control over allocation/deallocation
 * - No garbage collection pauses or unpredictable timing
 * - Each client struct is explicitly malloc'd and free'd
 *
 * DIRECT SYSTEM ACCESS:
 * - No runtime abstraction layer (unlike JVM, .NET, OCaml)
 * - Direct TCP socket operations through Mongoose
 * - Predictable performance characteristics
 *
 * EVENT-DRIVEN ARCHITECTURE:
 * - Single-threaded event loop (like Node.js but in C)
 * - No complex threading or async/await abstractions
 * - MG_EV_CLOSE fires immediately when TCP connection breaks
 *
 * COMPARISON WITH OTHER LANGUAGES:
 * - OCaml/Haskell: HTTP frameworks buffer responses, hide disconnections
 * - Rust: Complex Pin/Unpin memory safety, async runtime overhead
 * - C: Direct access to connection state, immediate cleanup possible
 */

#include "mongoose.h"
#include <cjson/cJSON.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <time.h>

#define PORT "5013"

// SSE Client data structure
typedef struct sse_client {
  char id[8];                 // Client ID (A, B, C, etc.)
  struct mg_connection *conn; // Mongoose connection pointer
  uint64_t connected_at;      // Connection timestamp in milliseconds
  struct sse_client *next;    // Linked list pointer
} sse_client_t;

// Global state
static sse_client_t *clients = NULL; // Linked list of active clients
static uint32_t client_counter = 0;  // For generating sequential IDs
static struct mg_mgr mgr;            // Mongoose event manager

// Get current timestamp in milliseconds
static uint64_t get_timestamp_ms(void) {
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return (uint64_t)(tv.tv_sec) * 1000 + (uint64_t)(tv.tv_usec) / 1000;
}

// Log message with timestamp
static void log_message(const char *msg) {
  time_t now = time(NULL);
  struct tm *utc = gmtime(&now);
  printf("[%04d-%02d-%02dT%02d:%02d:%02d.000Z] %s\n", utc->tm_year + 1900,
         utc->tm_mon + 1, utc->tm_mday, utc->tm_hour, utc->tm_min, utc->tm_sec,
         msg);
  fflush(stdout);
}

// Generate alphabetic ID (A, B, C, ..., Z, AA, AB, ...)
static void generate_alphabetic_id(uint32_t n, char *buffer) {
  buffer[0] = '\0';
  int pos = 0;

  do {
    buffer[pos++] = 'A' + (n % 26);
    n = n / 26;
  } while (n > 0 && pos < 7);

  // Reverse the string
  for (int i = 0; i < pos / 2; i++) {
    char temp = buffer[i];
    buffer[i] = buffer[pos - 1 - i];
    buffer[pos - 1 - i] = temp;
  }
  buffer[pos] = '\0';
}

// Add client to linked list
static void add_client(struct mg_connection *c, const char *client_id) {
  sse_client_t *client = (sse_client_t *)malloc(sizeof(sse_client_t));
  if (!client)
    return;

  strncpy(client->id, client_id, sizeof(client->id) - 1);
  client->id[sizeof(client->id) - 1] = '\0';
  client->conn = c;
  client->connected_at = get_timestamp_ms();
  client->next = clients;
  clients = client;

  // Store client pointer in connection user data
  c->fn_data = client;
}

// Count active clients
static int count_clients(void) {
  int count = 0;
  sse_client_t *current = clients;
  while (current) {
    count++;
    current = current->next;
  }
  return count;
}

// JSON helper functions using cJSON
static char *create_sse_message_json(const char *type, const char *client_id,
                                     const char *message, uint64_t timestamp,
                                     int total_clients) {
  cJSON *json = cJSON_CreateObject();
  if (!json)
    return NULL;

  // Always add type
  cJSON_AddStringToObject(json, "type", type);

  // Conditionally add other fields
  if (client_id) {
    cJSON_AddStringToObject(json, "clientId", client_id);
  }
  if (message) {
    cJSON_AddStringToObject(json, "message", message);
  }
  if (timestamp > 0) {
    cJSON_AddNumberToObject(json, "timestamp", (double)timestamp);
  }
  if (total_clients >= 0) {
    cJSON_AddNumberToObject(json, "totalClients", total_clients);
  }

  char *json_string = cJSON_PrintUnformatted(json);
  cJSON_Delete(json);
  return json_string; // caller must free()
}

static char *create_status_json(int active_connections,
                                sse_client_t *clients_list,
                                const char *implementation) {
  cJSON *json = cJSON_CreateObject();
  if (!json)
    return NULL;

  cJSON_AddNumberToObject(json, "activeConnections", active_connections);
  cJSON_AddStringToObject(json, "implementation", implementation);

  // Create clients array
  cJSON *clients_array = cJSON_CreateArray();
  sse_client_t *current = clients_list;
  while (current) {
    cJSON *client_obj = cJSON_CreateObject();
    cJSON_AddStringToObject(client_obj, "id", current->id);
    cJSON_AddNumberToObject(client_obj, "connectedAt",
                            (double)current->connected_at);
    cJSON_AddItemToArray(clients_array, client_obj);
    current = current->next;
  }
  cJSON_AddItemToObject(json, "clients", clients_array);

  char *json_string = cJSON_Print(json); // formatted for readability
  cJSON_Delete(json);
  return json_string; // caller must free()
}

// Remove client from linked list and free memory
static void remove_client(struct mg_connection *c) {
  if (!c->fn_data)
    return;

  sse_client_t *client = (sse_client_t *)c->fn_data;
  sse_client_t **current = &clients;

  // Find and remove from linked list
  while (*current) {
    if (*current == client) {
      *current = client->next;
      break;
    }
    current = &(*current)->next;
  }

  char msg[256];
  snprintf(msg, sizeof(msg), "Client %s removed. Remaining: %d", client->id,
           count_clients());
  log_message(msg);

  free(client);
  c->fn_data = NULL;
}

// Handle SSE endpoint
static void handle_sse(struct mg_connection *c,
                       struct mg_http_message *hm __attribute__((unused))) {
  // Generate client ID
  char client_id[8];
  generate_alphabetic_id(client_counter++, client_id);

  char msg[256];
  snprintf(msg, sizeof(msg), "Client %s connecting...", client_id);
  log_message(msg);

  // Add to client list
  add_client(c, client_id);

  snprintf(msg, sizeof(msg), "Client %s connected. Total clients: %d",
           client_id, count_clients());
  log_message(msg);

  // Send SSE headers
  mg_printf(c, "HTTP/1.1 200 OK\r\n"
               "Content-Type: text/event-stream\r\n"
               "Cache-Control: no-cache\r\n"
               "Connection: keep-alive\r\n"
               "Access-Control-Allow-Origin: *\r\n"
               "\r\n");

  // Send initial connection message using cJSON
  char *json_msg = create_sse_message_json(
      "connected", client_id, "C/Mongoose SSE connection established", 0, -1);
  if (json_msg) {
    mg_printf(c, "data: %s\n\n", json_msg);
    free(json_msg);
  }
}

// Handle status endpoint
static void handle_status(struct mg_connection *c,
                          struct mg_http_message *hm __attribute__((unused))) {
  int total_clients = count_clients();

  // Generate JSON response using cJSON
  char *json_response = create_status_json(
      total_clients, clients, "C/Mongoose (direct connection management)");
  if (json_response) {
    mg_printf(c,
              "HTTP/1.1 200 OK\r\n"
              "Content-Type: application/json\r\n"
              "\r\n%s",
              json_response);
    free(json_response);
  } else {
    // Fallback error response
    mg_printf(c, "HTTP/1.1 500 Internal Server Error\r\n"
                 "Content-Type: text/plain\r\n"
                 "\r\nJSON generation failed");
  }
}

// Send ping to all connected clients
static void send_ping_to_all(void) {
  uint64_t timestamp = get_timestamp_ms();
  int total_clients = count_clients();

  sse_client_t *current = clients;
  while (current) {
    // Generate ping message using cJSON
    char *ping_json = create_sse_message_json("ping", current->id, NULL,
                                              timestamp, total_clients);
    if (ping_json) {
      int sent = mg_printf(current->conn, "data: %s\n\n", ping_json);
      if (sent > 0) {
        char msg[128];
        snprintf(msg, sizeof(msg), "Ping sent to %s", current->id);
        log_message(msg);
      }
      free(ping_json);
    }

    current = current->next;
  }
}

// Main event handler
static void ev_handler(struct mg_connection *c, int ev, void *ev_data) {
  if (ev == MG_EV_HTTP_MSG) {
    struct mg_http_message *hm = (struct mg_http_message *)ev_data;

    if (mg_match(hm->uri, mg_str("/events"), NULL)) {
      handle_sse(c, hm);
    } else if (mg_match(hm->uri, mg_str("/status"), NULL)) {
      handle_status(c, hm);
    } else if (mg_match(hm->method, mg_str("OPTIONS"), NULL)) {
      // Handle CORS preflight
      mg_printf(c, "HTTP/1.1 200 OK\r\n"
                   "Access-Control-Allow-Origin: *\r\n"
                   "Access-Control-Allow-Methods: GET, OPTIONS\r\n"
                   "Access-Control-Allow-Headers: Content-Type\r\n"
                   "\r\n");
    } else {
      // 404 Not Found
      mg_printf(c, "HTTP/1.1 404 Not Found\r\n\r\n"
                   "Not Found\n\n"
                   "Available endpoints:\n"
                   "- /events (SSE)\n"
                   "- /status (JSON)\n");
    }
  } else if (ev == MG_EV_CLOSE) {
    // Connection closed - clean up client
    if (c->fn_data) {
      remove_client(c);
    }
  }
}

// Timer callback for sending pings
static void timer_callback(void *arg __attribute__((unused))) {
  send_ping_to_all();
}

int main(void) {
  printf("\n");
  printf("🔧 C/Mongoose SSE Server Starting (Port %s)\n", PORT);
  printf("✅ This implementation features:\n");
  printf("   - Event-driven architecture with direct TCP control\n");
  printf("   - Manual memory management for predictable performance\n");
  printf("   - Immediate disconnection detection via MG_EV_CLOSE\n");
  printf("   - No runtime abstraction - direct system access\n");
  printf("\n");
  printf("Expected behavior: Fastest disconnection detection\n");
  printf("\n");
  printf("Endpoints:\n");
  printf("  - http://localhost:%s/events (SSE stream)\n", PORT);
  printf("  - http://localhost:%s/status (Active connections)\n", PORT);

  // Initialize Mongoose manager
  mg_mgr_init(&mgr);

  // Create HTTP listener
  if (mg_http_listen(&mgr, "http://0.0.0.0:" PORT, ev_handler, NULL) == NULL) {
    fprintf(stderr, "Failed to create HTTP listener\n");
    return 1;
  }

  log_message("Server started successfully on port " PORT);

  // Set up 1-second timer for pings
  mg_timer_add(&mgr, 1000, MG_TIMER_REPEAT, timer_callback, NULL);

  // Main event loop
  for (;;) {
    mg_mgr_poll(&mgr, 1000);
  }

  mg_mgr_free(&mgr);
  return 0;
}