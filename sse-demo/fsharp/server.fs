open System
open System.Collections.Concurrent
open System.IO
open System.Net
open System.Text
open System.Text.Json
open System.Threading
open System.Threading.Tasks

// F# SSE server with proper disconnection detection
// This demonstrates that F# can handle SSE client disconnections properly

let port = 5006

// SSE Client data structure
type SSEClient =
    { Id: string
      Response: HttpListenerResponse
      ConnectedAt: DateTime
      CancellationToken: CancellationTokenSource
      mutable Active: bool }

let clients = ConcurrentDictionary<string, SSEClient>()
let mutable clientCounter = 0

// Generate alphabetic IDs (A, B, C, ..., Z, AA, AB, ...)
let generateAlphabeticId n =
    let rec go num acc =
        if num < 0 then
            acc
        else
            let remainder = num % 26
            let quotient = num / 26
            let char = char (65 + remainder) |> string

            if quotient = 0 then
                char + acc
            else
                go (quotient - 1) (char + acc)

    go n ""

// Log message with timestamp
let logMessage msg =
    let timestamp = DateTime.UtcNow.ToString("yyyy-MM-ddTHH:mm:ss.fffZ")
    printfn "[%s] %s" timestamp msg

// SSE Message types
type SSEConnectedMessage =
    { ``type``: string
      clientId: string
      message: string }

type SSEPingMessage =
    { ``type``: string
      timestamp: int64
      clientId: string
      totalClients: int }

// Create SSE message with proper JSON serialization
let createSSEMessage (data: obj) =
    let json = JsonSerializer.Serialize(data)
    $"data: {json}\n\n"

// Send SSE message
let sendSSEMessage (response: HttpListenerResponse) (message: string) =
    try
        let bytes = Encoding.UTF8.GetBytes(message)
        response.OutputStream.Write(bytes, 0, bytes.Length)
        response.OutputStream.Flush()
        true
    with ex ->
        false

// Cleanup client
let cleanup clientId =
    let (removed, client) = clients.TryRemove(clientId)

    if removed then
        client.Active <- false
        client.CancellationToken.Cancel()

        try
            client.Response.Close()
        with _ ->
            ()

        logMessage $"Client {clientId} removed. Remaining: {clients.Count}"

// Handle SSE events endpoint
let handleEvents (context: HttpListenerContext) =
    let clientId = generateAlphabeticId clientCounter
    clientCounter <- clientCounter + 1

    logMessage $"Client {clientId} connecting..."

    let response = context.Response
    response.ContentType <- "text/event-stream"
    response.Headers.Add("Cache-Control", "no-cache")
    response.Headers.Add("Connection", "keep-alive")
    response.Headers.Add("Access-Control-Allow-Origin", "*")
    response.StatusCode <- 200

    let cancellationToken = new CancellationTokenSource()

    let client =
        { Id = clientId
          Response = response
          ConnectedAt = DateTime.UtcNow
          CancellationToken = cancellationToken
          Active = true }

    clients.[clientId] <- client
    logMessage $"Client {clientId} connected. Total clients: {clients.Count}"

    // Send initial connection message
    let initialMessage =
        createSSEMessage
            { ``type`` = "connected"
              clientId = clientId
              message = "F# SSE connection established" }

    if not (sendSSEMessage response initialMessage) then
        cleanup clientId
    else
        // Start ping loop
        let pingTask =
            Task.Run(
                fun () ->
                    try
                        while client.Active && not cancellationToken.Token.IsCancellationRequested do
                            Thread.Sleep(1000)

                            if client.Active then
                                let timestamp = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()

                                let pingMessage =
                                    createSSEMessage
                                        { ``type`` = "ping"
                                          timestamp = timestamp
                                          clientId = clientId
                                          totalClients = clients.Count }

                                if sendSSEMessage response pingMessage then
                                    logMessage $"Ping sent to {clientId}"
                                else
                                    logMessage $"Error sending ping to {clientId}"
                                    cleanup clientId
                                    client.Active <- false
                    with
                    | :? OperationCanceledException -> logMessage $"Ping task cancelled for {clientId}"
                    | ex ->
                        logMessage $"Ping task error for {clientId}: {ex.Message}"
                        cleanup clientId
                , cancellationToken.Token
            )

        // Monitor for disconnection - F#/.NET will detect when client disconnects
        try
            try
                pingTask.Wait()
            with
            | :? AggregateException as ex -> logMessage $"Connection error for {clientId}: {ex.InnerException.Message}"
            | ex -> logMessage $"Connection error for {clientId}: {ex.Message}"
        finally
            cleanup clientId

// Handle status endpoint
let handleStatus (context: HttpListenerContext) =
    let response = context.Response
    response.ContentType <- "application/json"
    response.Headers.Add("Access-Control-Allow-Origin", "*")
    response.StatusCode <- 200

    let clientsInfo =
        clients.Values
        |> Seq.map (fun client ->
            let timestamp = ((DateTimeOffset client.ConnectedAt).ToUnixTimeMilliseconds())
            $"{{\"id\": \"{client.Id}\", \"connectedAt\": {timestamp}}}")
        |> String.concat ",\\n    "

    let statusJson =
        sprintf
            """{\n  "activeConnections": %d,\n  "clients": [\n    %s\n  ],\n  "implementation": "F# (proper disconnection detection)"\n}"""
            clients.Count
            clientsInfo

    let bytes = Encoding.UTF8.GetBytes(statusJson)
    response.OutputStream.Write(bytes, 0, bytes.Length)
    response.Close()

// Handle OPTIONS requests
let handleOptions (context: HttpListenerContext) =
    let response = context.Response
    response.Headers.Add("Access-Control-Allow-Origin", "*")
    response.Headers.Add("Access-Control-Allow-Methods", "GET, OPTIONS")
    response.Headers.Add("Access-Control-Allow-Headers", "Content-Type")
    response.StatusCode <- 200
    response.Close()

// Handle 404
let handle404 (context: HttpListenerContext) =
    let response = context.Response
    response.ContentType <- "text/plain"
    response.StatusCode <- 404

    let message =
        "Not Found\\n\\nAvailable endpoints:\\n- /events (SSE)\\n- /status (JSON)"

    let bytes = Encoding.UTF8.GetBytes(message)
    response.OutputStream.Write(bytes, 0, bytes.Length)
    response.Close()

// Main request handler
let handleRequest (context: HttpListenerContext) =
    let request = context.Request
    let path = request.Url.AbsolutePath

    match request.HttpMethod, path with
    | "GET", "/events" -> handleEvents context
    | "GET", "/status" -> handleStatus context
    | "OPTIONS", _ -> handleOptions context
    | _, _ -> handle404 context

[<EntryPoint>]
let main argv =
    let listener = new HttpListener()
    listener.Prefixes.Add($"http://localhost:{port}/")

    // Setup shutdown handler
    Console.CancelKeyPress.Add(fun args ->
        args.Cancel <- true
        logMessage "\\nShutting down..."

        // Close all client connections
        for kvp in clients do
            let client = kvp.Value
            logMessage $"Closing connection {client.Id}"

            try
                client.Active <- false
                client.CancellationToken.Cancel()
                client.Response.Close()
            with ex ->
                logMessage $"Error closing {client.Id}: {ex.Message}"

        clients.Clear()
        listener.Stop()
        logMessage "Server closed"
        Environment.Exit(0))

    printfn ""
    printfn "🔷 F# SSE Server Starting (Port %d)" port
    printfn "✅ This implementation has proper disconnection detection:"
    printfn "   - .NET HttpListener detects client disconnections through exceptions"
    printfn "   - Immediate disconnection detection through exception handling"
    printfn "   - Thread-safe client management with ConcurrentDictionary"
    printfn ""
    printfn "Expected behavior: Immediate and reliable disconnection detection"
    printfn ""
    printfn "Endpoints:"
    printfn "  - http://localhost:%d/events (SSE stream)" port
    printfn "  - http://localhost:%d/status (Active connections)" port

    try
        listener.Start()
        logMessage "Server started successfully"

        // Handle requests in a loop
        let mutable running = true

        while listener.IsListening && running do
            try
                let context = listener.GetContext()
                Task.Run(fun () -> handleRequest context) |> ignore
            with
            | :? ObjectDisposedException ->
                logMessage "Server stopped"
                running <- false
            | ex -> logMessage $"Error handling request: {ex.Message}"

        0
    with ex ->
        logMessage $"Server error: {ex.Message}"
        1
