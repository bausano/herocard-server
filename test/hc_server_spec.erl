-module(hc_server_spec).

-include_lib("eunit/include/eunit.hrl").

-import(hc_game, [create/1]).

-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

length_test() -> ?assert(3 =:= 3).

pls_test() -> ?assertEqual(ok, hc_game:create("")).

%% TEST MODULE.

start() ->
    {ok, Socket} =  gen_tcp:connect({127,0,0,1}, 5551, [binary, {active, false}]),
    spawn(fun() -> loop(Socket) end),
    ok = send(Socket, "This is a message"),
    send(Socket, ", another message."),
    timer:sleep(infinity).

send(Socket, Message) ->
    gen_tcp:send(Socket, Message).

loop(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {error, Reason} -> ?PRINT(Reason);
		{ok, Packet} ->
			?PRINT(Packet),
			loop(Socket)
	end.
