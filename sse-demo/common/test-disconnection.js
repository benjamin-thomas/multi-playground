#!/usr/bin/env node

/**
 * E2E Test Orchestrator for SSE Disconnection Detection
 * Tests each server implementation to verify proper client disconnection detection
 */

const { spawn } = require('child_process');
const http = require('http');
const net = require('net');
const EventSource = require('eventsource');
const path = require('path');
const fs = require('fs');

// Server configurations
const SERVERS = [
  { name: 'Node.js', port: 5000, dir: 'js', startCmd: 'node', args: ['server.js'], ready: 'Node.js SSE server running' },
  { name: 'PureScript FFI', port: 5001, dir: 'ps', startCmd: './start.sh', args: [], ready: 'PureScript SSE Server Starting' },
  { name: 'Go', port: 5002, dir: 'go', startCmd: './start.sh', args: [], ready: 'Go SSE Server Starting' },
  { name: 'Ruby', port: 5003, dir: 'ruby', startCmd: './start.sh', args: [], ready: 'Ruby SSE Server Starting' },
  { name: 'Java', port: 5004, dir: 'java', startCmd: './start.sh', args: [], ready: 'Java SSE Server Starting' },
  { name: 'Haskell', port: 5005, dir: 'haskell', startCmd: './start.sh', args: [], ready: 'Haskell SSE Server Starting' },
  { name: 'F#', port: 5006, dir: 'fsharp', startCmd: './start.sh', args: [], ready: 'F# SSE Server Starting' },
  { name: 'OCaml', port: 5007, dir: 'ocaml', startCmd: './start.sh', args: [], ready: 'OCaml SSE Server Starting' },
  { name: 'Python', port: 5008, dir: 'python', startCmd: './start.sh', args: [], ready: 'Python SSE Server Starting' },
  { name: 'C#', port: 5009, dir: 'csharp', startCmd: './start.sh', args: [], ready: 'C# SSE Server Starting' },
  { name: 'Elixir', port: 5010, dir: 'elixir', startCmd: './start.sh', args: [], ready: 'Elixir SSE Server Starting' },
  { name: 'Gleam', port: 5011, dir: 'gleam', startCmd: './start.sh', args: [], ready: 'Gleam SSE Server Starting' },
  { name: 'Rust', port: 5012, dir: 'rust', startCmd: './start.sh', args: [], ready: 'Rust SSE Server Starting' },
  { name: 'C', port: 5013, dir: 'c', startCmd: './start.sh', args: [], ready: 'C/Mongoose SSE Server Starting' },
  { name: 'Erlang', port: 5014, dir: 'erlang', startCmd: './start.sh', args: [], ready: 'Erlang SSE Server Starting' },
  { name: 'Express', port: 5015, dir: 'express', startCmd: './start.sh', args: [], ready: 'Express SSE Server Starting' },
];

// Test configuration
const TEST_CONFIG = {
  serverStartTimeout: 10000,  // 10 seconds to start
  connectionWaitTime: 3000,   // 3 seconds after connection
  disconnectWaitTime: 5000,   // 5 seconds to detect disconnection
  statusCheckInterval: 500,   // Check /status every 500ms
};

// ANSI color codes for output
const colors = {
  reset: '\x1b[0m',
  bright: '\x1b[1m',
  red: '\x1b[31m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  cyan: '\x1b[36m',
  gray: '\x1b[90m'
};

// Test results storage
const testResults = [];

/**
 * Check if a port is available (not occupied by another process)
 */
function checkPortAvailability(port) {
  return new Promise((resolve) => {
    const server = net.createServer();

    server.listen(port, () => {
      server.once('close', () => {
        resolve(true);  // Port is available
      });
      server.close();
    });

    server.on('error', () => {
      resolve(false);  // Port is occupied
    });
  });
}

/**
 * Check if server is responding on /status endpoint
 */
function checkServerStatus(port) {
  return new Promise((resolve) => {
    const options = {
      hostname: 'localhost',
      port: port,
      path: '/status',
      method: 'GET',
      timeout: 1000
    };

    const req = http.request(options, (res) => {
      resolve(res.statusCode === 200);
    });

    req.on('error', () => resolve(false));
    req.on('timeout', () => {
      req.destroy();
      resolve(false);
    });

    req.end();
  });
}

/**
 * Wait for server to be ready
 */
async function waitForServer(port, timeout = TEST_CONFIG.serverStartTimeout) {
  const startTime = Date.now();

  while (Date.now() - startTime < timeout) {
    if (await checkServerStatus(port)) {
      return true;
    }
    await new Promise(resolve => setTimeout(resolve, TEST_CONFIG.statusCheckInterval));
  }

  return false;
}

/**
 * Start a server and capture its output
 */
function startServer(server) {
  return new Promise((resolve, reject) => {
    console.log(`${colors.cyan}Starting ${server.name} server...${colors.reset}`);

    const serverPath = path.join(__dirname, '..', server.dir);
    const serverProcess = spawn(server.startCmd, server.args, {
      cwd: serverPath,
      shell: true,  // Use shell for better compatibility with .NET processes
      detached: true  // Create new process group for proper cleanup
    });

    let output = '';
    let errorOutput = '';
    let serverReady = false;

    // Capture stdout
    serverProcess.stdout.on('data', (data) => {
      const text = data.toString();
      output += text;

      // Log server output in gray
      process.stdout.write(`${colors.gray}[${server.name}] ${text}${colors.reset}`);

      // Check if server is ready
      if (!serverReady && text.includes(server.ready)) {
        serverReady = true;
      }
    });

    // Capture stderr
    serverProcess.stderr.on('data', (data) => {
      const text = data.toString();
      errorOutput += text;
      process.stdout.write(`${colors.yellow}[${server.name} ERR] ${text}${colors.reset}`);
    });

    serverProcess.on('error', (err) => {
      reject(new Error(`Failed to start ${server.name}: ${err.message}`));
    });

    // Wait for server to be ready
    waitForServer(server.port).then((isReady) => {
      if (isReady) {
        resolve({
          process: serverProcess,
          output: () => output,
          errorOutput: () => errorOutput,
          clearOutput: () => { output = ''; errorOutput = ''; }
        });
      } else {
        serverProcess.kill();
        reject(new Error(`${server.name} server failed to start within timeout`));
      }
    });
  });
}

/**
 * Test SSE connection and disconnection detection
 */
async function testDisconnection(server, serverHandle) {
  console.log(`${colors.blue}Testing ${server.name} disconnection detection...${colors.reset}`);

  const result = {
    server: server.name,
    port: server.port,
    connected: false,
    clientId: null,
    disconnectionDetected: false,
    disconnectionTime: null,
    error: null
  };

  try {
    // Clear previous output
    serverHandle.clearOutput();

    // Create SSE connection
    console.log(`  Connecting to http://localhost:${server.port}/events...`);
    const eventSource = new EventSource(`http://localhost:${server.port}/events`);

    let connectedPromise = new Promise((resolve, reject) => {
      const timeout = setTimeout(() => {
        reject(new Error('Connection timeout'));
      }, 5000);

      eventSource.onopen = () => {
        console.log(`  ${colors.green}✓${colors.reset} Connection opened`);
      };

      eventSource.onmessage = (event) => {
        try {
          const data = JSON.parse(event.data);
          if (data.type === 'connected' && data.clientId) {
            result.connected = true;
            result.clientId = data.clientId;
            console.log(`  ${colors.green}✓${colors.reset} Connected as client: ${data.clientId}`);
            clearTimeout(timeout);
            resolve();
          }
        } catch (e) {
          // Ignore parse errors
        }
      };

      eventSource.onerror = (err) => {
        clearTimeout(timeout);
        reject(new Error(`Connection error: ${err.message || 'Unknown error'}`));
      };
    });

    await connectedPromise;

    // Wait for stable connection
    console.log(`  Waiting for stable connection...`);
    await new Promise(resolve => setTimeout(resolve, TEST_CONFIG.connectionWaitTime));

    // Force disconnect
    console.log(`  ${colors.yellow}Disconnecting client...${colors.reset}`);
    const disconnectTime = Date.now();
    eventSource.close();

    // Monitor server output for disconnection detection
    console.log(`  Monitoring for disconnection detection...`);
    const checkStartTime = Date.now();

    while (Date.now() - checkStartTime < TEST_CONFIG.disconnectWaitTime) {
      const output = serverHandle.output();

      // Check for the universal disconnection pattern used by all servers
      if (result.clientId && output.includes(`Client ${result.clientId} removed`)) {
        result.disconnectionDetected = true;
        result.disconnectionTime = (Date.now() - disconnectTime) / 1000;
        console.log(`  ${colors.green}✓${colors.reset} Disconnection detected in ${result.disconnectionTime.toFixed(2)}s`);
        break;
      }

      await new Promise(resolve => setTimeout(resolve, 100));
    }

    if (!result.disconnectionDetected) {
      console.log(`  ${colors.red}✗${colors.reset} Disconnection NOT detected within ${TEST_CONFIG.disconnectWaitTime / 1000}s`);
    }

  } catch (error) {
    result.error = error.message;
    console.log(`  ${colors.red}✗${colors.reset} Test failed: ${error.message}`);
  }

  return result;
}

/**
 * Stop a server gracefully
 */
async function stopServer(serverHandle, name) {
  console.log(`${colors.cyan}Stopping ${name} server...${colors.reset}`);

  return new Promise((resolve) => {
    if (!serverHandle.process || serverHandle.process.killed) {
      resolve();
      return;
    }

    let forcedKill = false;
    let hasExited = false;

    const timeout = setTimeout(() => {
      forcedKill = true;
      console.log(`  ${colors.red}🚨 BUG: ${name} server didn't respond to SIGTERM after 5 seconds!${colors.reset}`);
      console.log(`  ${colors.yellow}Force killing ${name} server with SIGKILL${colors.reset}`);
      try {
        // Kill entire process group with SIGKILL
        process.kill(-serverHandle.process.pid, 'SIGKILL');
      } catch (e) {
        // Process group might already be dead, try individual process
        try {
          serverHandle.process.kill('SIGKILL');
        } catch (e2) {
          // Process might already be dead
        }
      }

      // Mark this as a server bug in the results
      if (!serverHandle.serverBugs) serverHandle.serverBugs = [];
      serverHandle.serverBugs.push('Server does not respond to SIGTERM (graceful shutdown failed)');

      resolve();
    }, 5000); // 5 seconds as requested

    const onExit = () => {
      if (hasExited) return; // Prevent duplicate execution
      hasExited = true;

      clearTimeout(timeout);
      if (!forcedKill) {
        console.log(`  ${colors.green}✓${colors.reset} ${name} server stopped gracefully`);
      }
      resolve();
    };

    serverHandle.process.once('exit', onExit);
    serverHandle.process.once('close', onExit);

    try {
      // Kill entire process group with SIGTERM first
      process.kill(-serverHandle.process.pid, 'SIGTERM');
    } catch (e) {
      // Process group might not exist, try individual process
      try {
        serverHandle.process.kill('SIGTERM');
      } catch (e2) {
        clearTimeout(timeout);
        resolve();
      }
    }
  });
}

/**
 * Run tests for a single server
 */
async function testServer(server) {
  console.log(`\n${colors.bright}${'='.repeat(60)}${colors.reset}`);
  console.log(`${colors.bright}Testing: ${server.name} (Port ${server.port})${colors.reset}`);
  console.log(`${'='.repeat(60)}`);

  let serverHandle = null;
  let result = null;

  try {
    // Check if port is available first
    const isPortAvailable = await checkPortAvailability(server.port);
    if (!isPortAvailable) {
      throw new Error(`Port ${server.port} is already in use! Please kill any processes using this port first.`);
    }

    // Start the server
    serverHandle = await startServer(server);

    // Run disconnection test
    result = await testDisconnection(server, serverHandle);

  } catch (error) {
    result = {
      server: server.name,
      port: server.port,
      connected: false,
      clientId: null,
      disconnectionDetected: false,
      disconnectionTime: null,
      error: error.message
    };
    console.log(`${colors.red}Error: ${error.message}${colors.reset}`);
  } finally {
    // Stop the server
    if (serverHandle) {
      await stopServer(serverHandle, server.name);

      // Add any server bugs to the result
      if (serverHandle.serverBugs && serverHandle.serverBugs.length > 0) {
        if (!result) result = {
          server: server.name,
          port: server.port,
          connected: false,
          clientId: null,
          disconnectionDetected: false,
          disconnectionTime: null,
          error: null
        };
        result.serverBugs = serverHandle.serverBugs;
      }
    }
  }

  testResults.push(result);
  return result;
}

/**
 * Print test results summary
 */
function printResults() {
  console.log(`\n${colors.bright}${'='.repeat(60)}${colors.reset}`);
  console.log(`${colors.bright}TEST RESULTS SUMMARY${colors.reset}`);
  console.log(`${'='.repeat(60)}\n`);

  // Create results table
  console.log('Server          | Port | Connected | Disconnection | Time    | Status');
  console.log('----------------|------|-----------|---------------|---------|--------');

  for (const result of testResults) {
    const name = result.server.padEnd(14);
    const port = result.port.toString().padEnd(4);
    const connected = result.connected ? '✓' : '✗';
    const disconnected = result.disconnectionDetected ? '✓' : '✗';
    const time = result.disconnectionTime ? `${result.disconnectionTime.toFixed(2)}s` : 'N/A';

    let status = '';
    if (result.error) {
      status = `${colors.red}FAILED${colors.reset}`;
    } else if (result.disconnectionDetected) {
      status = `${colors.green}PASSED${colors.reset}`;
    } else if (result.connected) {
      status = `${colors.yellow}NO DETECT${colors.reset}`;
    } else {
      status = `${colors.red}FAILED${colors.reset}`;
    }

    console.log(`${name}  | ${port} | ${connected.padEnd(9)} | ${disconnected.padEnd(13)} | ${time.padEnd(7)} | ${status}`);

    if (result.error) {
      console.log(`  ${colors.gray}Error: ${result.error}${colors.reset}`);
    }

    if (result.serverBugs && result.serverBugs.length > 0) {
      result.serverBugs.forEach(bug => {
        console.log(`  ${colors.red}🚨 Bug: ${bug}${colors.reset}`);
      });
    }
  }

  // Summary statistics
  const passed = testResults.filter(r => r.disconnectionDetected).length;
  const failed = testResults.filter(r => !r.disconnectionDetected).length;
  const errors = testResults.filter(r => r.error).length;

  console.log(`\n${colors.bright}Summary:${colors.reset}`);
  console.log(`  ${colors.green}Passed: ${passed}${colors.reset}`);
  console.log(`  ${colors.yellow}Failed to detect: ${failed - errors}${colors.reset}`);
  console.log(`  ${colors.red}Errors: ${errors}${colors.reset}`);
  console.log(`  Total: ${testResults.length}`);
}

/**
 * Main test runner
 */
async function main() {
  console.log(`${colors.bright}SSE Disconnection Detection Test Suite${colors.reset}`);
  console.log(`Testing ${SERVERS.length} server implementations\n`);

  // Check if we should test specific servers only
  const args = process.argv.slice(2);
  let serversToTest = SERVERS;

  if (args.length > 0) {
    const requestedServers = args.map(arg => {
      // Handle port numbers
      const port = parseInt(arg);
      if (!isNaN(port)) {
        return SERVERS.find(s => s.port === port);
      }

      // Handle directory paths (e.g., ../haskell/, ../haskell, haskell, ./haskell)
      if (arg.includes('/')) {
        const dirName = arg.replace(/\/$/, '').split('/').pop(); // Remove trailing slash and get last part
        return SERVERS.find(s => s.dir === dirName);
      }

      // Try to match by name or directory
      return SERVERS.find(s =>
        s.name.toLowerCase() === arg.toLowerCase() ||
        s.dir.toLowerCase() === arg.toLowerCase()
      );
    }).filter(s => s !== null && s !== undefined);

    if (requestedServers.length > 0) {
      serversToTest = requestedServers;
      console.log(`Testing only: ${serversToTest.map(s => s.name).join(', ')}\n`);
    } else {
      console.log(`No servers found matching: ${args.join(', ')}`);
      console.log(`Available servers: ${SERVERS.map(s => `${s.name} (${s.dir})`).join(', ')}`);
      process.exit(1);
    }
  }

  // Run tests sequentially
  for (const server of serversToTest) {
    try {
      await testServer(server);
    } catch (error) {
      console.error(`${colors.red}Failed to test ${server.name}: ${error.message}${colors.reset}`);
    }

    // Small delay between tests
    await new Promise(resolve => setTimeout(resolve, 1000));
  }

  // Print results
  printResults();

  // Calculate exit code based on test results
  const passed = testResults.filter(r => r.disconnectionDetected).length;
  const failed = testResults.filter(r => !r.disconnectionDetected).length;
  const exitCode = failed > 0 ? 1 : 0;

  // Force exit to ensure the script terminates
  console.log(`\n${colors.gray}Test suite completed. Exiting with code ${exitCode}...${colors.reset}`);
  process.exit(exitCode);
}

// Run the tests
main().catch(error => {
  console.error(`${colors.red}Fatal error: ${error.message}${colors.reset}`);
  process.exit(1);
});