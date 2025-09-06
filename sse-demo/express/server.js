#!/usr/bin/env node

const express = require('express');
const app = express();
const PORT = 5015;

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

// CORS middleware
app.use((req, res, next) => {
  res.header('Access-Control-Allow-Origin', '*');
  res.header('Access-Control-Allow-Headers', 'Content-Type');
  res.header('Access-Control-Allow-Methods', 'GET, POST, OPTIONS');
  next();
});

// Hello endpoint
app.get('/hello', (req, res) => {
  console.log(`[${Date.now()}] ${req.method} ${req.url}`);

  res.json({
    message: 'Hello from Express!',
    implementation: 'Express.js',
    timestamp: Date.now()
  });
});

// Status endpoint
app.get('/status', (req, res) => {
  console.log(`[${Date.now()}] ${req.method} ${req.url}`);

  const status = {
    activeConnections: clients.size,
    clients: Array.from(clients.values()).map(c => ({
      id: c.id,
      connectedAt: c.connectedAt.getTime()
    })),
    implementation: 'Express.js'
  };

  res.json(status);
});

// SSE events endpoint
app.get('/events', (req, res) => {
  console.log(`[${Date.now()}] ${req.method} ${req.url}`);

  const clientId = generateAlphabeticId(clientIdCounter++);
  console.log(`[Express] Client ${clientId} connecting...`);

  // Set SSE headers
  res.writeHead(200, {
    'Content-Type': 'text/event-stream',
    'Cache-Control': 'no-cache',
    'Connection': 'keep-alive'
  });

  // Store client
  const client = {
    id: clientId,
    res: res,
    req: req,
    connectedAt: new Date()
  };
  clients.set(clientId, client);
  console.log(`Client ${clientId} connected. Total clients: ${clients.size}`);

  // Send initial connection message
  const connMsg = {
    type: 'connected',
    clientId: clientId,
    message: 'Express SSE connection established'
  };
  res.write(`data: ${JSON.stringify(connMsg)}\n\n`);

  // Send periodic pings
  const pingInterval = setInterval(() => {
    if (!clients.has(clientId)) {
      clearInterval(pingInterval);
      return;
    }

    try {
      const pingData = {
        type: 'ping',
        clientId: clientId,
        timestamp: Date.now(),
        totalClients: clients.size
      };
      res.write(`data: ${JSON.stringify(pingData)}\n\n`);
      console.log(`[${Date.now()}] Ping sent to ${clientId}`);
    } catch (error) {
      console.log(`[${Date.now()}] Error sending ping to ${clientId}:`, error.message);
      cleanup();
    }
  }, 1000);

  // Cleanup function
  function cleanup() {
    clearInterval(pingInterval);
    clients.delete(clientId);
    console.log(`Client ${clientId} removed. Remaining: ${clients.size}`);

    try {
      if (!res.headersSent) {
        res.end();
      }
    } catch (e) {
      console.log(`Error ending response for ${clientId}:`, e.message);
    }
  }

  // CRITICAL: Handle disconnection
  // Express-specific: req.on('close') is most reliable
  req.on('close', () => {
    console.log(`[${Date.now()}] Client ${clientId} disconnected`);
    cleanup();
  });

  // Also handle response errors
  res.on('error', (error) => {
    console.log(`[${Date.now()}] Response error for ${clientId}:`, error.message);
    cleanup();
  });
});

// 404 handler
app.use((req, res) => {
  console.log(`[${Date.now()}] ${req.method} ${req.url}`);
  res.status(404).send('Not Found\n\nAvailable endpoints:\n- /events (SSE)\n- /status (JSON)\n- /hello (JSON)');
});

// Start server
const server = app.listen(PORT, () => {
  console.log('');
  console.log('🚂 Express SSE Server Starting (Port 5015)');
  console.log(`🚂 Express SSE Server started on port ${PORT}`);
  console.log('✅ Using Express.js framework:');
  console.log('   - Express middleware and routing');
  console.log('   - Built on Node.js HTTP module');
  console.log('   - req.on("close") for disconnection detection');
  console.log('');
  console.log('Expected behavior: IMMEDIATE disconnection detection');
  console.log('');
  console.log('Endpoints:');
  console.log(`  - http://localhost:${PORT}/events (SSE stream)`);
  console.log(`  - http://localhost:${PORT}/status (Active connections)`);
  console.log(`  - http://localhost:${PORT}/hello (JSON greeting)`);
});

// Cleanup on exit
process.once('SIGINT', () => {
  console.log('\nShutting down Express server...');

  // Close all client connections
  clients.forEach((client, id) => {
    console.log(`Closing connection ${id}`);
    try {
      client.res.end();
    } catch (e) {
      console.log(`Error closing ${id}:`, e.message);
    }
  });
  clients.clear();

  server.close(() => {
    console.log('Express server closed');
    process.exit(0);
  });

  setTimeout(() => {
    console.log('Force exit');
    process.exit(1);
  }, 2000);
});