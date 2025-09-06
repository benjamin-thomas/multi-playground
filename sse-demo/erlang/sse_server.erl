-module(sse_server).
-export([start/0, start/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-behaviour(gen_server).

-record(state, {
    clients = #{},
    counter = 0,
    listen_socket
}).

-record(client, {
    id,
    connected_at,
    socket,
    monitor_ref
}).

%% Public API
start() ->
    start(5014).

start(Port) ->
    io:format("~n"),
    io:format("📡 Erlang SSE Server Starting (Port ~p)~n", [Port]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

%% Gen Server Callbacks
init([Port]) ->
    process_flag(trap_exit, true),
    case
        gen_tcp:listen(Port, [
            binary,
            {packet, 0},
            {active, false},
            {reuseaddr, true},
            {backlog, 100}
        ])
    of
        {ok, ListenSocket} ->
            io:format("📡 Erlang SSE Server started on port ~p~n", [Port]),
            io:format("✅ Using raw TCP sockets with OTP:~n"),
            io:format("   - Raw TCP for maximum control~n"),
            io:format("   - Process monitoring for disconnection detection~n"),
            io:format("   - Immediate socket close detection~n"),
            io:format("~n"),
            io:format("Expected behavior: IMMEDIATE disconnection detection~n"),
            io:format("~n"),
            io:format("Endpoints:~n"),
            io:format("  - http://localhost:~p/events (SSE stream)~n", [Port]),
            io:format("  - http://localhost:~p/status (Active connections)~n", [Port]),
            io:format("  - http://localhost:~p/hello (JSON greeting)~n", [Port]),
            spawn_link(fun() -> accept_loop(ListenSocket) end),
            {ok, #state{listen_socket = ListenSocket}};
        {error, Reason} ->
            {stop, {listen_failed, Reason}}
    end.

handle_call(
    {add_client, ClientSocket}, _From, State = #state{clients = Clients, counter = Counter}
) ->
    ClientId = generate_client_id(Counter),
    ConnectedAt = erlang:system_time(millisecond),
    % Monitor the client process
    MonitorRef = monitor(process, self()),
    Client = #client{
        id = ClientId,
        connected_at = ConnectedAt,
        socket = ClientSocket,
        monitor_ref = MonitorRef
    },
    NewClients = maps:put(ClientId, Client, Clients),
    NewState = State#state{clients = NewClients, counter = Counter + 1},
    TotalClients = maps:size(NewClients),
    io:format("Client ~s connected. Total clients: ~p~n", [ClientId, TotalClients]),
    {reply, {ok, Client}, NewState};
handle_call({remove_client, ClientId}, _From, State = #state{clients = Clients}) ->
    case maps:find(ClientId, Clients) of
        {ok, #client{monitor_ref = MonitorRef}} ->
            demonitor(MonitorRef, [flush]),
            NewClients = maps:remove(ClientId, Clients),
            NewState = State#state{clients = NewClients},
            RemainingClients = maps:size(NewClients),
            io:format("Client ~s removed. Remaining: ~p~n", [ClientId, RemainingClients]),
            {reply, ok, NewState};
        error ->
            {reply, ok, State}
    end;
handle_call(get_clients, _From, State = #state{clients = Clients}) ->
    ClientsList = maps:values(Clients),
    {reply, ClientsList, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _MonitorRef, process, _Pid, _Reason}, State) ->
    %% Client process died, we'll clean it up when we detect the disconnection
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{listen_socket = ListenSocket}) ->
    gen_tcp:close(ListenSocket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Functions
accept_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, ClientSocket} ->
            spawn(fun() -> handle_client(ClientSocket) end),
            accept_loop(ListenSocket);
        {error, closed} ->
            ok;
        {error, Reason} ->
            io:format("Accept error: ~p~n", [Reason]),
            accept_loop(ListenSocket)
    end.

handle_client(Socket) ->
    case recv_http_request(Socket) of
        {ok, Method, Path, _Headers} ->
            Timestamp = erlang:system_time(millisecond),
            io:format("[~p] ~s ~s~n", [Timestamp, Method, Path]),
            handle_request(Socket, Method, Path);
        {error, Reason} ->
            io:format("Failed to parse HTTP request: ~p~n", [Reason]),
            gen_tcp:close(Socket)
    end.

recv_http_request(Socket) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, Data} ->
            parse_http_request(binary_to_list(Data));
        {error, Reason} ->
            {error, Reason}
    end.

parse_http_request(Data) ->
    Lines = string:split(Data, "\r\n", all),
    case Lines of
        [RequestLine | HeaderLines] ->
            case string:split(RequestLine, " ", all) of
                [Method, Path | _] ->
                    Headers = parse_headers(HeaderLines),
                    {ok, Method, Path, Headers};
                _ ->
                    {error, invalid_request_line}
            end;
        _ ->
            {error, invalid_request}
    end.

parse_headers(Lines) ->
    parse_headers(Lines, []).

parse_headers([], Acc) ->
    Acc;
parse_headers(["" | _], Acc) ->
    Acc;
parse_headers([Line | Rest], Acc) ->
    case string:split(Line, ": ", leading) of
        [Name, Value] ->
            parse_headers(Rest, [{string:lowercase(Name), Value} | Acc]);
        _ ->
            parse_headers(Rest, Acc)
    end.

handle_request(Socket, "GET", "/hello") ->
    Response = #{
        <<"message">> => <<"Hello from Erlang!">>,
        <<"implementation">> => <<"Erlang OTP + raw TCP">>,
        <<"timestamp">> => erlang:system_time(millisecond)
    },
    JsonResponse = encode_json(Response),
    send_http_response(Socket, 200, "application/json", JsonResponse),
    gen_tcp:close(Socket);
handle_request(Socket, "GET", "/status") ->
    Clients = gen_server:call(?MODULE, get_clients),
    ClientsJson = lists:map(
        fun(#client{id = Id, connected_at = ConnectedAt}) ->
            #{
                <<"id">> => list_to_binary(Id),
                <<"connectedAt">> => ConnectedAt
            }
        end,
        Clients
    ),

    Status = #{
        <<"activeConnections">> => length(Clients),
        <<"clients">> => ClientsJson,
        <<"implementation">> => <<"Erlang OTP + raw TCP">>
    },
    JsonResponse = encode_json(Status),
    send_http_response(Socket, 200, "application/json", JsonResponse),
    gen_tcp:close(Socket);
handle_request(Socket, "GET", "/events") ->
    {ok, Client} = gen_server:call(?MODULE, {add_client, Socket}),
    #client{id = ClientId} = Client,

    io:format("[Erlang] Client ~s connecting...~n", [ClientId]),

    %% Send SSE headers
    send_sse_headers(Socket),

    %% Send initial connection message
    ConnectedMsg = #{
        <<"type">> => <<"connected">>,
        <<"clientId">> => list_to_binary(ClientId),
        <<"message">> => <<"Erlang SSE connection established">>
    },
    send_sse_event(Socket, encode_json(ConnectedMsg)),

    %% Start ping loop with disconnection detection
    sse_loop(Socket, ClientId);
handle_request(Socket, _Method, Path) ->
    NotFoundMsg =
        "Not Found\n\nAvailable endpoints:\n- /events (SSE)\n- /status (JSON)\n- /hello (JSON)",
    send_http_response(Socket, 404, "text/plain", NotFoundMsg),
    gen_tcp:close(Socket).

send_http_response(Socket, StatusCode, ContentType, Body) ->
    StatusText =
        case StatusCode of
            200 -> "OK";
            404 -> "Not Found";
            _ -> "Unknown"
        end,
    Response = io_lib:format(
        "HTTP/1.1 ~p ~s\r\n"
        "Content-Type: ~s\r\n"
        "Access-Control-Allow-Origin: *\r\n"
        "Content-Length: ~p\r\n"
        "\r\n"
        "~s",
        [StatusCode, StatusText, ContentType, iolist_size(Body), Body]
    ),
    gen_tcp:send(Socket, Response).

send_sse_headers(Socket) ->
    Headers =
        "HTTP/1.1 200 OK\r\n"
        "Content-Type: text/event-stream\r\n"
        "Cache-Control: no-cache\r\n"
        "Connection: keep-alive\r\n"
        "Access-Control-Allow-Origin: *\r\n"
        "\r\n",
    gen_tcp:send(Socket, Headers).

send_sse_event(Socket, Data) ->
    Event = io_lib:format("data: ~s\n\n", [Data]),
    gen_tcp:send(Socket, Event).

sse_loop(Socket, ClientId) ->
    %% Send ping every 1000ms
    timer:sleep(1000),

    case gen_tcp:send(Socket, "") of
        ok ->
            %% Socket is still open, send ping
            Clients = gen_server:call(?MODULE, get_clients),
            PingMsg = #{
                <<"type">> => <<"ping">>,
                <<"clientId">> => list_to_binary(ClientId),
                <<"timestamp">> => erlang:system_time(millisecond),
                <<"totalClients">> => length(Clients)
            },
            case send_sse_event(Socket, encode_json(PingMsg)) of
                ok ->
                    Timestamp = erlang:system_time(millisecond),
                    io:format("[~p] Ping sent to ~s~n", [Timestamp, ClientId]),
                    sse_loop(Socket, ClientId);
                {error, _} ->
                    handle_client_disconnect(Socket, ClientId)
            end;
        {error, _} ->
            handle_client_disconnect(Socket, ClientId)
    end.

handle_client_disconnect(Socket, ClientId) ->
    Timestamp = erlang:system_time(millisecond),
    io:format("[~p] Client ~s disconnected~n", [Timestamp, ClientId]),
    gen_server:call(?MODULE, {remove_client, ClientId}),
    gen_tcp:close(Socket).

generate_client_id(N) ->
    generate_client_id(N, "").

generate_client_id(N, Acc) when N < 0 ->
    Acc;
generate_client_id(N, Acc) ->
    Remainder = N rem 26,
    Quotient = N div 26,
    Char = [Remainder + $A],
    if
        Quotient == 0 ->
            Char ++ Acc;
        true ->
            generate_client_id(Quotient - 1, Char ++ Acc)
    end.

%% Simple JSON encoder
encode_json(Map) when is_map(Map) ->
    Pairs = maps:fold(
        fun(K, V, Acc) ->
            Key = encode_json_value(K),
            Value = encode_json_value(V),
            [Key ++ ":" ++ Value | Acc]
        end,
        [],
        Map
    ),
    "{" ++ string:join(lists:reverse(Pairs), ",") ++ "}";
encode_json(List) when is_list(List) ->
    Values = lists:map(fun encode_json_value/1, List),
    "[" ++ string:join(Values, ",") ++ "]";
encode_json(Value) ->
    encode_json_value(Value).

encode_json_value(Bin) when is_binary(Bin) ->
    "\"" ++ binary_to_list(Bin) ++ "\"";
encode_json_value(Str) when is_list(Str) ->
    "\"" ++ Str ++ "\"";
encode_json_value(Num) when is_integer(Num) ->
    integer_to_list(Num);
encode_json_value(Num) when is_float(Num) ->
    float_to_list(Num);
encode_json_value(true) ->
    "true";
encode_json_value(false) ->
    "false";
encode_json_value(null) ->
    "null";
encode_json_value(Map) when is_map(Map) ->
    encode_json(Map);
encode_json_value(List) when is_list(List) ->
    encode_json(List).
