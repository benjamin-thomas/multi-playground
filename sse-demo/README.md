# Server-Sent Events (SSE) Disconnection Detection: A Multi-Language Analysis

## Overview

This project emerged from limitations encountered while developing a PureScript fullstack application ([pfm project](https://github.com/benjamin-thomas/pfm)). The goal was to systematically evaluate how different programming languages and their respective HTTP libraries handle Server-Sent Events (SSE) client disconnection detection.

The core challenge: **How quickly can a server detect when an SSE client disconnects?**

## Motivation

Modern web applications rely heavily on real-time communication. SSE provides a simple, efficient mechanism for server-to-client streaming, but proper resource cleanup requires immediate disconnection detection. Poor disconnection handling leads to:

- Memory leaks from orphaned connections
- Resource exhaustion under load
- Degraded server performance
- Unnecessary network traffic

This study explores how various language ecosystems handle this fundamental requirement.

## Methodology

### Test Framework

Each implementation provides three endpoints:
- `/events` - SSE stream with 1-second ping intervals
- `/status` - JSON status of active connections  
- `/hello` - Simple JSON greeting

The test framework:
1. Establishes SSE connections
2. Abruptly terminates client connections
3. Measures server response time to detect disconnection
4. Reports results with precision timing

### Implementation Requirements

All servers implement identical functionality:
- Alphabetic client ID generation (A, B, C...)
- Connection lifecycle logging to STDOUT
- 1-second ping intervals with client count
- Proper CORS headers
- Graceful shutdown handling

**Note on STDOUT Usage**: The testing approach relies heavily on STDOUT for observability, requiring careful flushing in some implementations. This design choice enables linear observation of server behavior but may not reflect production logging practices.

## Results Summary

### ✅ Excellent Performance (≤0.10s disconnection detection)

| Language/Framework | Detection Time | Notes |
|-------------------|----------------|--------|
| **Node.js (raw HTTP)** | ~0.10s | Direct access to request events |
| **PureScript FFI** | ~0.10s | Custom FFI to Node.js primitives |
| **Go net/http** | ~0.10s | Context-based connection management |
| **Express.js** | ~0.10s | Built on Node.js, preserves event access |
| **Erlang OTP** | Immediate | Raw TCP sockets, process monitoring |

### ❌ Poor Performance or Failures

| Language/Framework | Issue | Root Cause |
|-------------------|-------|------------|
| **PureScript node-http** | Compile errors | Missing types, API mismatches |
| **PureScript Express** | Package unavailable | Ecosystem abandonment |
| **Various high-level frameworks** | Delayed detection | Abstraction layers hiding connection events |

## Key Findings

### 1. Abstraction vs. Control Trade-offs

**Working implementations** share common characteristics:
- Direct access to underlying connection events
- Minimal abstraction between application and socket layer
- Explicit connection lifecycle management

**Failing implementations** typically suffer from:
- High-level abstractions hiding critical events
- Framework-imposed patterns preventing low-level access
- Documentation-reality gaps

### 2. Library Ecosystem Health

The study revealed significant disparities in ecosystem maintenance:

**Robust Ecosystems:**
- **Go**: Standard library "just works"
- **Node.js**: Mature, stable APIs
- **Erlang**: Battle-tested, minimal dependencies

**Problematic Ecosystems:**
- **PureScript**: Package set mismatches, abandoned libraries
- **Complex frameworks**: Over-abstraction of core functionality

### 3. The TCP-Level Solution

Several implementations required dropping to TCP-level programming due to HTTP library limitations. This suggests that proper SSE disconnection detection is a specialized requirement poorly served by general-purpose HTTP abstractions.

## Technical Insights

### Why High-Level Libraries Fail

Most HTTP libraries optimize for request-response patterns, not persistent connections. Common issues include:

1. **Buffering**: Output buffering delays disconnection signals
2. **Event Abstraction**: Connection events hidden behind framework APIs  
3. **Lifecycle Management**: Automatic resource cleanup interfering with detection
4. **Threading Models**: Disconnection events lost across thread boundaries

### The Goldilocks Zone

Effective SSE requires libraries that are:
- **Not too low-level**: Raw sockets are unnecessarily complex
- **Not too high-level**: Request/response abstractions hide connection state
- **Just right**: HTTP server with exposed connection events

## Implementation Notes

### Code Quality Disclaimer

This codebase was heavily AI-assisted and serves demonstration purposes. While functional for comparative analysis, the implementations likely contain defects and non-idiomatic patterns. Production use would require significant refinement.

### Language-Specific Observations

**Go**: The standard library's simplicity and reliability stood out. Context-based cancellation provided elegant disconnection handling.

**Erlang**: Raw TCP approach eliminated all abstraction issues. Process monitoring ensured immediate detection but required more low-level code.

**Node.js/Express**: Mature ecosystem with predictable behavior. The transition from raw Node.js to Express demonstrated how thin wrappers can preserve essential functionality.

**PureScript**: Ecosystem fragmentation and maintenance issues significantly impacted development velocity. Custom FFI proved more reliable than library abstractions.

## Broader Implications

This study highlights a recurring theme in software development: **abstractions that optimize for common cases often break uncommon but critical requirements**.

SSE disconnection detection represents a class of problems where:
- The requirement seems simple
- High-level abstractions promise easy solutions
- Reality requires understanding lower-level mechanisms
- Success depends on tool/library design philosophy

## Future Work

This project serves as a reference for:
- Evaluating HTTP library capabilities beyond basic functionality
- Understanding language ecosystem maturity and maintenance
- Demonstrating the limits of abstraction in systems programming

The complexity of implementing seemingly simple SSE functionality effectively stress-tested each language's ecosystem, exposing both strengths and weaknesses that may not be apparent in simpler applications.

## Usage

Each implementation can be tested individually:

```bash
# Test a specific implementation
cd common && node test-disconnection.js ../go

# Test all implementations  
cd common && node test-disconnection.js
```

The test suite provides detailed timing analysis and helps identify which approaches provide reliable disconnection detection for production SSE applications.