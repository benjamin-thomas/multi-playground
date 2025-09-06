package main

import (
	"context"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"sync"
	"time"
)

type Client struct {
	ID          string    `json:"id"`
	ConnectedAt time.Time `json:"connectedAt"`
	writer      http.ResponseWriter
	ctx         context.Context
	cancel      context.CancelFunc
}

type Server struct {
	clients   map[string]*Client
	clientsMu sync.RWMutex
	counter   int
}

func NewServer() *Server {
	return &Server{
		clients: make(map[string]*Client),
		counter: 0,
	}
}

// Generate alphabetic IDs (A, B, C, ..., Z, AA, AB, ...)
func (s *Server) generateClientID() string {
	id := ""
	n := s.counter
	s.counter++

	for {
		remainder := n % 26
		char := string(rune('A' + remainder))
		id = char + id
		n = n / 26
		if n == 0 {
			break
		}
		n--
	}
	return id
}

func (s *Server) addClient(w http.ResponseWriter) *Client {
	s.clientsMu.Lock()
	defer s.clientsMu.Unlock()

	ctx, cancel := context.WithCancel(context.Background())
	client := &Client{
		ID:          s.generateClientID(),
		ConnectedAt: time.Now(),
		writer:      w,
		ctx:         ctx,
		cancel:      cancel,
	}

	s.clients[client.ID] = client
	fmt.Printf("Client %s connected. Total clients: %d\n", client.ID, len(s.clients))
	return client
}

func (s *Server) removeClient(clientID string) {
	s.clientsMu.Lock()
	defer s.clientsMu.Unlock()

	if client, exists := s.clients[clientID]; exists {
		client.cancel()
		delete(s.clients, clientID)
		fmt.Printf("Client %s removed. Remaining: %d\n", clientID, len(s.clients))
	}
}

func (s *Server) getClients() []*Client {
	s.clientsMu.RLock()
	defer s.clientsMu.RUnlock()

	clients := make([]*Client, 0, len(s.clients))
	for _, client := range s.clients {
		clients = append(clients, client)
	}
	return clients
}

func (s *Server) handleHello(w http.ResponseWriter, r *http.Request) {
	log.Printf("[%d] %s %s", time.Now().UnixMilli(), r.Method, r.URL.Path)

	w.Header().Set("Content-Type", "application/json")
	w.Header().Set("Access-Control-Allow-Origin", "*")

	response := map[string]interface{}{
		"message":        "Hello from Go!",
		"implementation": "Go net/http",
		"timestamp":      time.Now().UnixMilli(),
	}

	json.NewEncoder(w).Encode(response)
}

func (s *Server) handleStatus(w http.ResponseWriter, r *http.Request) {
	log.Printf("[%d] %s %s", time.Now().UnixMilli(), r.Method, r.URL.Path)

	w.Header().Set("Content-Type", "application/json")
	w.Header().Set("Access-Control-Allow-Origin", "*")

	clients := s.getClients()
	clientsData := make([]map[string]interface{}, len(clients))

	for i, client := range clients {
		clientsData[i] = map[string]interface{}{
			"id":          client.ID,
			"connectedAt": client.ConnectedAt.UnixMilli(),
		}
	}

	status := map[string]interface{}{
		"activeConnections": len(clients),
		"clients":           clientsData,
		"implementation":    "Go net/http",
	}

	json.NewEncoder(w).Encode(status)
}

func (s *Server) handleSSE(w http.ResponseWriter, r *http.Request) {
	log.Printf("[%d] %s %s", time.Now().UnixMilli(), r.Method, r.URL.Path)

	// Set SSE headers
	w.Header().Set("Content-Type", "text/event-stream")
	w.Header().Set("Cache-Control", "no-cache")
	w.Header().Set("Connection", "keep-alive")
	w.Header().Set("Access-Control-Allow-Origin", "*")

	// Add client
	client := s.addClient(w)

	log.Printf("[Go] Client %s connecting...", client.ID)

	// Send initial connection message
	connMsg := map[string]interface{}{
		"type":     "connected",
		"clientId": client.ID,
		"message":  "Go SSE connection established",
	}
	s.writeSSE(w, connMsg)

	// Setup cleanup on connection close
	notify := r.Context().Done()
	go func() {
		<-notify
		currentTime := time.Now().UnixMilli()
		fmt.Printf("[%d] Client %s disconnected\n", currentTime, client.ID)
		s.removeClient(client.ID)
	}()

	// Ping loop
	ticker := time.NewTicker(1 * time.Second)
	defer ticker.Stop()

	for {
		select {
		case <-client.ctx.Done():
			return
		case <-ticker.C:
			clients := s.getClients()
			pingMsg := map[string]any{
				"type":         "ping",
				"clientId":     client.ID,
				"timestamp":    time.Now().UnixMilli(),
				"totalClients": len(clients),
			}

			if err := s.writeSSE(w, pingMsg); err != nil {
				fmt.Printf("[%d] Error writing to client %s: %v\n", time.Now().UnixMilli(), client.ID, err)
				s.removeClient(client.ID)
				return
			}

			fmt.Printf("[%d] Ping sent to %s\n", time.Now().UnixMilli(), client.ID)

			// Force flush
			if flusher, ok := w.(http.Flusher); ok {
				flusher.Flush()
			}
		}
	}
}

func (s *Server) writeSSE(w http.ResponseWriter, data any) error {
	jsonData, err := json.Marshal(data)
	if err != nil {
		return err
	}

	_, err = fmt.Fprintf(w, "data: %s\n\n", string(jsonData))
	return err
}

func (s *Server) handleRequest(w http.ResponseWriter, r *http.Request) {
	switch r.URL.Path {
	case "/hello":
		s.handleHello(w, r)
	case "/status":
		s.handleStatus(w, r)
	case "/events":
		s.handleSSE(w, r)
	default:
		log.Printf("[%d] %s %s", time.Now().UnixMilli(), r.Method, r.URL.Path)
		w.Header().Set("Content-Type", "text/plain")
		w.WriteHeader(http.StatusNotFound)
		w.Write([]byte("Not Found\n\nAvailable endpoints:\n- /events (SSE)\n- /status (JSON)\n- /hello (JSON)"))
	}
}

func main() {
	server := NewServer()

	log.Println("")
	log.Println("🐹 Go SSE Server Starting (Port 5002)")

	http.HandleFunc("/", server.handleRequest)

	log.Println("🐹 Go SSE Server started on port 5002")
	log.Println("✅ Using Go net/http package:")
	log.Println("   - Go handles HTTP server and routing")
	log.Println("   - Context-based connection management")
	log.Println("   - Request context cancellation for disconnection detection")
	log.Println("")
	log.Println("Expected behavior: IMMEDIATE disconnection detection")
	log.Println("")
	log.Println("Endpoints:")
	log.Println("  - http://localhost:5002/events (SSE stream)")
	log.Println("  - http://localhost:5002/status (Active connections)")
	log.Println("  - http://localhost:5002/hello (JSON greeting)")

	if err := http.ListenAndServe(":5002", nil); err != nil {
		log.Fatal("Server failed to start:", err)
	}
}
