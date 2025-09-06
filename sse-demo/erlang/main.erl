-module(main).
-export([start/0]).

start() ->
    sse_server:start().