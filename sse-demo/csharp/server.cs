using System;
using System.Collections.Concurrent;
using System.IO;
using System.Net;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Text.Json;

/// <summary>
/// C# SSE server with proper disconnection detection
/// This demonstrates that C# can handle SSE client disconnections properly
/// </summary>
public class Program
{
    private const int Port = 5009;
    private static readonly ConcurrentDictionary<string, SSEClient> Clients = new();
    private static int _clientCounter = 0;

    public class SSEClient
    {
        public string Id { get; set; }
        public HttpListenerResponse Response { get; set; }
        public DateTime ConnectedAt { get; set; }
        public CancellationTokenSource CancellationToken { get; set; }
        public bool Active { get; set; } = true;
    }

    // Generate alphabetic IDs (A, B, C, ..., Z, AA, AB, ...)
    private static string GenerateAlphabeticId(int n)
    {
        var result = "";
        var num = n;
        do
        {
            var remainder = num % 26;
            var quotient = num / 26;
            var character = (char)(65 + remainder);
            result = character + result;
            num = quotient - 1;
        } while (num >= 0);

        return result;
    }

    private static void LogMessage(string message)
    {
        var timestamp = DateTime.UtcNow.ToString("yyyy-MM-ddTHH:mm:ss.fffZ");
        Console.WriteLine($"[{timestamp}] {message}");
    }

    private static void Cleanup(string clientId)
    {
        if (Clients.TryRemove(clientId, out var client))
        {
            client.Active = false;
            client.CancellationToken?.Cancel();
            try
            {
                client.Response?.Close();
            }
            catch (Exception ex)
            {
                LogMessage($"Error closing response for {clientId}: {ex.Message}");
            }
            LogMessage($"Client {clientId} removed. Remaining: {Clients.Count}");
        }
    }

    private static async Task PingLoop(SSEClient client)
    {
        try
        {
            while (client.Active && !client.CancellationToken.Token.IsCancellationRequested)
            {
                await Task.Delay(1000, client.CancellationToken.Token);

                if (!client.Active) break;

                var timestamp = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds();
                var pingData = new
                {
                    type = "ping",
                    timestamp = timestamp,
                    clientId = client.Id,
                    totalClients = Clients.Count
                };

                var pingMessage = $"data: {JsonSerializer.Serialize(pingData)}\n\n";

                try
                {
                    var bytes = Encoding.UTF8.GetBytes(pingMessage);
                    await client.Response.OutputStream.WriteAsync(bytes, 0, bytes.Length, client.CancellationToken.Token);
                    await client.Response.OutputStream.FlushAsync(client.CancellationToken.Token);
                    LogMessage($"Ping sent to {client.Id}");
                }
                catch (Exception ex)
                {
                    LogMessage($"Error sending ping to {client.Id}: {ex.Message}");
                    Cleanup(client.Id);
                    break;
                }
            }
        }
        catch (OperationCanceledException)
        {
            LogMessage($"Ping task cancelled for {client.Id}");
        }
        catch (Exception ex)
        {
            LogMessage($"Ping task error for {client.Id}: {ex.Message}");
            Cleanup(client.Id);
        }
    }

    private static async Task HandleEvents(HttpListenerContext context)
    {
        var clientId = GenerateAlphabeticId(Interlocked.Increment(ref _clientCounter) - 1);
        LogMessage($"Client {clientId} connecting...");

        var response = context.Response;
        response.ContentType = "text/event-stream";
        response.Headers.Add("Cache-Control", "no-cache");
        response.Headers.Add("Connection", "keep-alive");
        response.Headers.Add("Access-Control-Allow-Origin", "*");
        response.StatusCode = 200;

        var cancellationToken = new CancellationTokenSource();
        var client = new SSEClient
        {
            Id = clientId,
            Response = response,
            ConnectedAt = DateTime.UtcNow,
            CancellationToken = cancellationToken
        };

        Clients[clientId] = client;
        LogMessage($"Client {clientId} connected. Total clients: {Clients.Count}");

        // Send initial connection message
        var initialData = new
        {
            type = "connected",
            clientId = clientId,
            message = "C# SSE connection established"
        };

        try
        {
            var initialMessage = $"data: {JsonSerializer.Serialize(initialData)}\n\n";
            var bytes = Encoding.UTF8.GetBytes(initialMessage);
            await response.OutputStream.WriteAsync(bytes);
            await response.OutputStream.FlushAsync();
        }
        catch (Exception ex)
        {
            LogMessage($"Error sending initial message to {clientId}: {ex.Message}");
            Cleanup(clientId);
            return;
        }

        // Start ping loop
        var pingTask = PingLoop(client);

        try
        {
            // Monitor for disconnection - C#/.NET will detect when client disconnects
            await pingTask;
        }
        catch (Exception ex)
        {
            LogMessage($"Connection error for {clientId}: {ex.Message}");
        }
        finally
        {
            Cleanup(clientId);
        }
    }

    private static async Task HandleStatus(HttpListenerContext context)
    {
        var response = context.Response;
        response.ContentType = "application/json";
        response.Headers.Add("Access-Control-Allow-Origin", "*");
        response.StatusCode = 200;

        var clientsInfo = new List<object>();
        foreach (var client in Clients.Values)
        {
            clientsInfo.Add(new
            {
                id = client.Id,
                connectedAt = ((DateTimeOffset)client.ConnectedAt).ToUnixTimeMilliseconds()
            });
        }

        var status = new
        {
            activeConnections = Clients.Count,
            clients = clientsInfo,
            implementation = "C# (proper disconnection detection)"
        };

        var json = JsonSerializer.Serialize(status, new JsonSerializerOptions { WriteIndented = true });
        var bytes = Encoding.UTF8.GetBytes(json);
        await response.OutputStream.WriteAsync(bytes, 0, bytes.Length);
        response.Close();
    }

    private static void HandleOptions(HttpListenerContext context)
    {
        var response = context.Response;
        response.Headers.Add("Access-Control-Allow-Origin", "*");
        response.Headers.Add("Access-Control-Allow-Methods", "GET, OPTIONS");
        response.Headers.Add("Access-Control-Allow-Headers", "Content-Type");
        response.StatusCode = 200;
        response.Close();
    }

    private static void Handle404(HttpListenerContext context)
    {
        var response = context.Response;
        response.ContentType = "text/plain";
        response.StatusCode = 404;
        var message = "Not Found\n\nAvailable endpoints:\n- /events (SSE)\n- /status (JSON)";
        var bytes = Encoding.UTF8.GetBytes(message);
        response.OutputStream.Write(bytes, 0, bytes.Length);
        response.Close();
    }

    private static async Task HandleRequest(HttpListenerContext context)
    {
        var request = context.Request;
        var path = request.Url.AbsolutePath;
        var method = request.HttpMethod;

        try
        {
            switch (method, path)
            {
                case ("GET", "/events"):
                    await HandleEvents(context);
                    break;
                case ("GET", "/status"):
                    await HandleStatus(context);
                    break;
                case ("OPTIONS", _):
                    HandleOptions(context);
                    break;
                default:
                    Handle404(context);
                    break;
            }
        }
        catch (Exception ex)
        {
            LogMessage($"Error handling request {method} {path}: {ex.Message}");
        }
    }

    public static async Task Main(string[] args)
    {
        var listener = new HttpListener();
        listener.Prefixes.Add($"http://localhost:{Port}/");

        // Setup shutdown handler
        Console.CancelKeyPress += (sender, e) =>
        {
            e.Cancel = true;
            LogMessage("\nShutting down...");

            // Close all client connections
            foreach (var client in Clients.Values)
            {
                LogMessage($"Closing connection {client.Id}");
                try
                {
                    client.Active = false;
                    client.CancellationToken?.Cancel();
                    client.Response?.Close();
                }
                catch (Exception ex)
                {
                    LogMessage($"Error closing {client.Id}: {ex.Message}");
                }
            }

            Clients.Clear();
            listener.Stop();
            LogMessage("Server closed");
            Environment.Exit(0);
        };

        Console.WriteLine();
        Console.WriteLine($"💙 C# SSE Server Starting (Port {Port})");
        Console.WriteLine("✅ This implementation has proper disconnection detection:");
        Console.WriteLine("   - .NET HttpListener detects client disconnections through exceptions");
        Console.WriteLine("   - Immediate disconnection detection through exception handling");
        Console.WriteLine("   - Thread-safe client management with ConcurrentDictionary");
        Console.WriteLine();
        Console.WriteLine("Expected behavior: Immediate and reliable disconnection detection");
        Console.WriteLine();
        Console.WriteLine("Endpoints:");
        Console.WriteLine($"  - http://localhost:{Port}/events (SSE stream)");
        Console.WriteLine($"  - http://localhost:{Port}/status (Active connections)");

        try
        {
            listener.Start();
            LogMessage("Server started successfully");

            // Handle requests
            while (listener.IsListening)
            {
                try
                {
                    var context = await listener.GetContextAsync();
                    _ = Task.Run(() => HandleRequest(context)); // Fire and forget
                }
                catch (ObjectDisposedException)
                {
                    LogMessage("Server stopped");
                    break;
                }
                catch (Exception ex)
                {
                    LogMessage($"Error accepting request: {ex.Message}");
                }
            }
        }
        catch (Exception ex)
        {
            LogMessage($"Server error: {ex.Message}");
        }
    }
}