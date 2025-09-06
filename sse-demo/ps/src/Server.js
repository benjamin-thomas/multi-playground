/**
 * FFI module for PureScript SSE Server
 * This handles all the Node.js HTTP server plumbing
 * PureScript only handles the business logic
 */

import http from 'http';
import url from 'url';

// Create raw HTTP server with PureScript request handler
export const createServerJS = (requestHandler) => () => {
  const server = http.createServer((req, res) => {
    requestHandler(req)(res)();
  });

  return server;
};

// Start listening on port
export const listenJS = (port) => (server) => (callback) => () =>
  server.listen(port, callback);

// SSE and Status endpoints are now handled in PureScript

// HTTP response functions for PureScript
export const writeHeadJS = (res) => (statusCode) => (headers) => () => {
  res.writeHead(statusCode, headers);
};

export const endResponseJS = (res) => (data) => () => {
  res.end(data);
};

export const writeResponseJS = (res) => (data) => () => {
  res.write(data);
};

export const jsonStringifyJS = (obj) => {
  return JSON.stringify(obj);
};

// Timer management
export const setIntervalJS = (ms) => (callback) => () => {
  return setInterval(callback, ms);
};

export const clearIntervalJS = (intervalId) => () => {
  clearInterval(intervalId);
};

// Event listeners
export const onRequestJS = (req) => (eventName) => (callback) => () => {
  req.on(eventName, callback);
};

export const onResponseJS = (res) => (eventName) => (callback) => () => {
  res.on(eventName, (error) => {
    // For error events, pass the error info; for others, just call callback
    if (eventName === 'error' && error) {
      console.log(`Response error: ${error.message}`);
    }
    callback();
  });
};

// Response state checking
export const isResponseClosedJS = (res) => () => {
  return res.destroyed || res.finished;
};

export const tryEndResponseJS = (res) => () => {
  try {
    if (!res.destroyed && !res.finished) {
      res.end();
    }
  } catch (e) {
    console.log(`Error ending response: ${e.message}`);
  }
};

// SSE connection management is now handled entirely in PureScript

// Helper to close server
export const closeServerJS = (server) => (callback) => () => {
  server.close(() => {
    callback();
  });
};

// Request parsing functions
export const getRequestUrlJS = (req) => () => {
  return req.url || '/';
};

export const getRequestMethodJS = (req) => () => {
  return req.method || 'GET';
};

export const parseUrlJS = (urlString) => () => {
  // FIXME: handle throws
  const parsed = url.parse(urlString, true);
  return {
    pathname: parsed.pathname || '/',
    query: parsed.query || {}
  };
};