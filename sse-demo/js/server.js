#!/usr/bin/env node

/**
 * Working Node.js SSE server with proper disconnection detection
 * This demonstrates the correct way to handle SSE client disconnections
 */

const http = require('http');
const url = require('url');

const PORT = 5000;

// Store active SSE connections
const clients = new Map();
let clientIdCounter = 0;

// Generate alphabetic IDs (A, B, C, ..., Z, AA, AB, ...)
function generateAlphabeticId(n) {
  let result = '';
  while (n >= 0) {
    result = String.fromCharCode(65 + (n % 26)) + result;
    n = Math.floor(n / 26) - 1;
    if (n < 0) break;
  }
  return result;
}

/**
 * Handle SSE connection with proper disconnection detection
 */
function handleSSE(req, res) {
  const clientId = generateAlphabeticId(clientIdCounter++);
  console.log(`[${new Date().toISOString()}] Client ${clientId} connecting...`);

  // Set SSE headers
  res.writeHead(200, {
    'Content-Type': 'text/event-stream',
    'Cache-Control': 'no-cache',
    'Connection': 'keep-alive',
    'Access-Control-Allow-Origin': '*'
  });

  // Store client
  const client = {
    id: clientId,
    res: res,
    req: req,
    connectedAt: new Date()
  };
  clients.set(clientId, client);
  console.log(`[${new Date().toISOString()}] Client ${clientId} connected. Total clients: ${clients.size}`);

  // Send initial connection message
  res.write(`data: {"type":"connected","clientId":"${clientId}","message":"Connection established"}\n\n`);

  // Send periodic pings
  const pingInterval = setInterval(() => {
    if (!clients.has(clientId)) {
      clearInterval(pingInterval);
      return;
    }

    try {
      const pingData = {
        type: 'ping',
        timestamp: Date.now(),
        clientId: clientId,
        totalClients: clients.size
      };
      res.write(`data: ${JSON.stringify(pingData)}\n\n`);
      console.log(`[${new Date().toISOString()}] Ping sent to ${clientId}`);
    } catch (error) {
      console.log(`[${new Date().toISOString()}] Error sending ping to ${clientId}:`, error.message);
      cleanup();
    }
  }, 1000);

  // Cleanup function
  function cleanup() {
    console.log(`[${new Date().toISOString()}] Cleaning up client ${clientId}`);
    clearInterval(pingInterval);
    clients.delete(clientId);
    console.log(`[${new Date().toISOString()}] Client ${clientId} removed. Remaining: ${clients.size}`);

    try {
      if (!res.destroyed && !res.finished) {
        res.end();
      }
    } catch (e) {
      console.log(`[${new Date().toISOString()}] Error ending response for ${clientId}:`, e.message);
    }
  }

  // CRITICAL: These event listeners enable proper disconnection detection

  // Most reliable for detecting client disconnection
  req.on('close', () => {
    console.log(`[${new Date().toISOString()}] Request 'close' event for ${clientId}`);
    cleanup();
  });

  // Fired when request ends normally
  req.on('end', () => {
    console.log(`[${new Date().toISOString()}] Request 'end' event for ${clientId}`);
    cleanup();
  });

  // Response close event
  res.on('close', () => {
    console.log(`[${new Date().toISOString()}] Response 'close' event for ${clientId}`);
    cleanup();
  });

  // Response error
  res.on('error', (error) => {
    console.log(`[${new Date().toISOString()}] Response error for ${clientId}:`, error.message);
    cleanup();
  });

  // Connection abort (older Node.js versions)
  req.on('aborted', () => {
    console.log(`[${new Date().toISOString()}] Request aborted for ${clientId}`);
    cleanup();
  });
}

/**
 * Handle status endpoint
 */
function handleStatus(req, res) {
  res.writeHead(200, {
    'Content-Type': 'application/json',
    'Access-Control-Allow-Origin': '*'
  });

  const status = {
    activeConnections: clients.size,
    clients: Array.from(clients.values()).map(c => ({
      id: c.id,
      connectedAt: c.connectedAt
    }))
  };

  res.end(JSON.stringify(status, null, 2));
}

/**
 * Main request handler
 */
const server = http.createServer((req, res) => {
  const parsedUrl = url.parse(req.url, true);

  console.log(`[${new Date().toISOString()}] ${req.method} ${req.url}`);

  switch (parsedUrl.pathname) {
    case '/events':
      handleSSE(req, res);
      break;
    case '/status':
      handleStatus(req, res);
      break;
    default:
      res.writeHead(404, { 'Content-Type': 'text/plain' });
      res.end('Not Found\n\nAvailable endpoints:\n- /events (SSE)\n- /status (JSON)');
  }
});

// Only start the server if this is the main module
if (require.main === module) {
  server.listen(PORT, () => {
    console.log(`[${new Date().toISOString()}] Node.js SSE server running on http://localhost:${PORT}`);
    console.log(`[${new Date().toISOString()}] Endpoints:`);
    console.log(`  - http://localhost:${PORT}/events (SSE stream)`);
    console.log(`  - http://localhost:${PORT}/status (Active connections)`);
  });
}

// Cleanup on exit
process.once('SIGINT', () => {
  console.log(`\n[${new Date().toISOString()}] Shutting down...`);

  // Close all client connections
  clients.forEach((client, id) => {
    console.log(`[${new Date().toISOString()}] Closing connection ${id}`);
    try {
      client.res.end();
    } catch (e) {
      console.log(`Error closing ${id}:`, e.message);
    }
  });
  clients.clear();

  server.close(() => {
    console.log(`[${new Date().toISOString()}] Server closed`);
    process.exit(0);
  });

  setTimeout(() => {
    console.log(`[${new Date().toISOString()}] Force exit`);
    process.exit(1);
  }, 2000);
});