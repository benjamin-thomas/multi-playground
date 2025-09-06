/**
 * Pure FFI for Node.js HTTP Server with proper SSE disconnection detection
 * This demonstrates that PureScript can handle disconnections properly when
 * we have access to the underlying Node.js request/response objects
 */

const http = require('http');
const url = require('url');

// Create HTTP server with direct access to Node.js objects
export const createSSEServer = (port) => (onRequest) => () => {
  const server = http.createServer((req, res) => {
    const parsedUrl = url.parse(req.url, true);
    
    // Log the request
    console.log(`[${new Date().toISOString()}] ${req.method} ${req.url}`);
    
    // Call PureScript handler with the request info and raw objects
    onRequest({
      method: req.method,
      url: req.url,
      pathname: parsedUrl.pathname,
      rawRequest: req,
      rawResponse: res
    })();
  });

  server.listen(port, () => {
    console.log(`[${new Date().toISOString()}] Pure FFI SSE server running on http://localhost:${port}`);
  });

  return server;
};

// Helper to generate alphabetic IDs
export const generateAlphabeticId = (n) => {
  let result = '';
  let num = n;
  while (num >= 0) {
    result = String.fromCharCode(65 + (num % 26)) + result;
    num = Math.floor(num / 26) - 1;
    if (num < 0) break;
  }
  return result;
};

// Setup SSE headers and initial connection
export const setupSSEConnection = (response) => (clientId) => () => {
  response.writeHead(200, {
    'Content-Type': 'text/event-stream',
    'Cache-Control': 'no-cache',
    'Connection': 'keep-alive',
    'Access-Control-Allow-Origin': '*'
  });

  // Send initial connection message
  const initialMsg = {
    type: 'connected',
    clientId: clientId,
    message: 'Pure FFI SSE with proper disconnection detection'
  };
  
  response.write(`data: ${JSON.stringify(initialMsg)}\\n\\n`);
  return response;
};

// Attach proper disconnection event listeners - THIS IS THE KEY DIFFERENCE
export const attachDisconnectionHandlers = (request) => (response) => (clientId) => (onDisconnect) => () => {
  let cleaned = false;
  
  const cleanup = () => {
    if (cleaned) return;
    cleaned = true;
    
    console.log(`[${new Date().toISOString()}] [FFI] Cleaning up client ${clientId}`);
    
    // Call PureScript cleanup function
    onDisconnect();
    
    // Try to end response if still open
    try {
      if (!response.destroyed && !response.finished) {
        response.end();
      }
    } catch (e) {
      console.log(`[${new Date().toISOString()}] Error ending response for ${clientId}:`, e.message);
    }
  };

  // CRITICAL: These event listeners enable immediate disconnection detection
  
  // Most reliable for detecting client disconnection
  request.on('close', () => {
    console.log(`[${new Date().toISOString()}] [FFI] Request 'close' event for ${clientId}`);
    cleanup();
  });

  // Fired when request ends normally  
  request.on('end', () => {
    console.log(`[${new Date().toISOString()}] [FFI] Request 'end' event for ${clientId}`);
    cleanup();
  });

  // Response close event
  response.on('close', () => {
    console.log(`[${new Date().toISOString()}] [FFI] Response 'close' event for ${clientId}`);
    cleanup();
  });

  // Response error
  response.on('error', (error) => {
    console.log(`[${new Date().toISOString()}] [FFI] Response error for ${clientId}:`, error.message);
    cleanup();
  });

  // Connection abort (older Node.js versions)
  request.on('aborted', () => {
    console.log(`[${new Date().toISOString()}] [FFI] Request aborted for ${clientId}`);
    cleanup();
  });

  // Return cleanup function for manual cleanup if needed
  return cleanup;
};

// Send SSE ping message
export const sendSSEPing = (response) => (clientId) => (timestamp) => (totalClients) => () => {
  try {
    const pingData = {
      type: 'ping',
      clientId: clientId,
      timestamp: timestamp,
      totalClients: totalClients
    };
    
    response.write(`data: ${JSON.stringify(pingData)}\\n\\n`);
    console.log(`[${new Date().toISOString()}] [FFI] Ping sent to ${clientId}`);
    return { success: true };
  } catch (error) {
    console.log(`[${new Date().toISOString()}] [FFI] Error sending ping to ${clientId}:`, error.message);
    return { success: false, error: error.message };
  }
};

// Send JSON response
export const sendJSONResponse = (response) => (json) => () => {
  response.writeHead(200, { 
    'Content-Type': 'application/json',
    'Access-Control-Allow-Origin': '*'
  });
  response.end(json);
};

// Send 404 response
export const send404Response = (response) => () => {
  response.writeHead(404, { 'Content-Type': 'text/plain' });
  response.end('Not Found\\n\\nAvailable endpoints:\\n- /events (SSE)\\n- /status (JSON)');
};

// Get current timestamp
export const getCurrentTimestamp = () => Date.now();