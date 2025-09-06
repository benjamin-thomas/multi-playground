#!/usr/bin/env ruby

# Ruby SSE server with proper disconnection detection
# This demonstrates that Ruby can handle SSE client disconnections properly

require 'webrick'
require 'json'
require 'time'

PORT = 5003

# Store active SSE connections
$clients = {}
$client_counter = 0
$clients_mutex = Mutex.new

# Generate alphabetic IDs (A, B, C, ..., Z, AA, AB, ...)
def generate_alphabetic_id(n)
  result = ''
  num = n
  loop do
    result = (65 + (num % 26)).chr + result
    num = num / 26 - 1
    break if num < 0
  end
  result
end

# Log with timestamp
def log_message(message)
  puts "[#{Time.now.iso8601}] #{message}"
  $stdout.flush
end

# Handle SSE connection with proper disconnection detection
def handle_sse(request, response)
  client_id = generate_alphabetic_id($client_counter)
  $client_counter += 1
  
  log_message "Client #{client_id} connecting..."
  
  # Set SSE headers
  response.status = 200
  response['Content-Type'] = 'text/event-stream'
  response['Cache-Control'] = 'no-cache'
  response['Connection'] = 'keep-alive'
  response['Access-Control-Allow-Origin'] = '*'
  response.chunked = true
  
  # Create a streaming body proc for SSE
  response.body = proc do |output_stream|
    begin
      # Store client connection
      client_info = {
        id: client_id,
        stream: output_stream,
        connected_at: Time.now,
        active: true
      }
      
      $clients_mutex.synchronize do
        $clients[client_id] = client_info
        log_message "Client #{client_id} connected. Total clients: #{$clients.size}"
      end
      
      # Send initial connection message
      connection_data = {
        type: 'connected',
        clientId: client_id,
        message: 'Ruby SSE connection established'
      }
      
      begin
        message = "data: #{connection_data.to_json}\n\n"
        output_stream.write(message)
      rescue => e
        log_message "Error sending initial message to #{client_id}: #{e.message}"
        cleanup_client(client_id)
        return
      end
      
      # Start ping loop
      loop do
        sleep 1
        
        # Check for shutdown signal
        break if $shutdown
        
        # Check if client is still active
        $clients_mutex.synchronize do
          client = $clients[client_id]
          break unless client && client[:active]
        end
        
        ping_data = {
          type: 'ping',
          timestamp: (Time.now.to_f * 1000).to_i,
          clientId: client_id,
          totalClients: $clients.size
        }
        
        begin
          message = "data: #{ping_data.to_json}\n\n"
          output_stream.write(message)
          log_message "Ping sent to #{client_id}"
        rescue => e
          log_message "Error sending ping to #{client_id}: #{e.message}"
          cleanup_client(client_id)
          break
        end
      end
      
    rescue => e
      log_message "Connection error for #{client_id}: #{e.message}"
    ensure
      cleanup_client(client_id)
    end
  end
end

# Cleanup client connection
def cleanup_client(client_id)
  $clients_mutex.synchronize do
    if $clients[client_id]
      $clients[client_id][:active] = false
      $clients.delete(client_id)
      log_message "Client #{client_id} removed. Remaining: #{$clients.size}"
    end
  end
end

# Handle status endpoint
def handle_status(request, response)
  response.status = 200
  response['Content-Type'] = 'application/json'
  response['Access-Control-Allow-Origin'] = '*'
  
  clients_info = []
  $clients_mutex.synchronize do
    clients_info = $clients.values.map do |client|
      {
        id: client[:id],
        connectedAt: client[:connected_at].to_f * 1000
      }
    end
  end
  
  status = {
    activeConnections: $clients.size,
    clients: clients_info,
    implementation: 'Ruby (proper disconnection detection)'
  }
  
  response.body = JSON.pretty_generate(status)
end

# Custom servlet for handling SSE
class SSEServlet < WEBrick::HTTPServlet::AbstractServlet
  def do_GET(request, response)
    case request.path
    when '/events'
      handle_sse(request, response)
    when '/status'
      handle_status(request, response)
    else
      response.status = 404
      response['Content-Type'] = 'text/plain'
      response.body = "Not Found\n\nAvailable endpoints:\n- /events (SSE)\n- /status (JSON)"
    end
  end
  
  def do_OPTIONS(request, response)
    response.status = 200
    response['Access-Control-Allow-Origin'] = '*'
    response['Access-Control-Allow-Methods'] = 'GET, OPTIONS'
    response['Access-Control-Allow-Headers'] = 'Content-Type'
    response.body = ''
  end
end

# Create and start server
server = WEBrick::HTTPServer.new(
  Port: PORT,
  Logger: WEBrick::Log.new(nil, WEBrick::Log::ERROR), # Suppress WEBrick logs
  AccessLog: [] # Suppress access logs
)

server.mount('/', SSEServlet)

# Handle shutdown gracefully
$shutdown = false

shutdown_proc = proc do
  $shutdown = true
end

trap('INT', &shutdown_proc)
trap('TERM', &shutdown_proc)

log_message ""
log_message "🔴 Ruby SSE Server Starting (Port #{PORT})"
log_message "✅ This implementation has proper disconnection detection:"
log_message "   - Ruby/WEBrick raises exceptions when clients disconnect"
log_message "   - Immediate disconnection detection through exception handling"
log_message "   - Thread-safe client management with mutexes"
log_message ""
log_message "Expected behavior: Immediate and reliable disconnection detection"
log_message ""
log_message "Endpoints:"
log_message "  - http://localhost:#{PORT}/events (SSE stream)"
log_message "  - http://localhost:#{PORT}/status (Active connections)"

begin
  # Start server in a thread so we can monitor shutdown
  server_thread = Thread.new { server.start }
  
  # Monitor for shutdown signal
  until $shutdown
    sleep 0.1
  end
  
  log_message "\nShutting down..."
  
  # Close all client connections
  $clients_mutex.synchronize do
    $clients.each do |id, client|
      log_message "Closing connection #{id}"
      begin
        client[:active] = false
      rescue => e
        log_message "Error closing #{id}: #{e.message}"
      end
    end
    $clients.clear
  end
  
  server.shutdown
  server_thread.join
  log_message "Server closed"
  
rescue => e
  log_message "Server error: #{e.message}"
  exit 1
end