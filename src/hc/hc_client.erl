-module(hc_client).

-export([start/0]).

%% Debug function.
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

start() ->
    {ok, Socket1} =  gen_tcp:connect({127,0,0,1}, 5551, [binary, {active, false}]),
    {ok, Socket2} =  gen_tcp:connect({127,0,0,1}, 5551, [binary, {active, false}]),
    spawn(fun() -> loop(Socket1) end),
    spawn(fun() -> loop(Socket2) end),
    register(director, spawn(fun() ->
      director(Socket1, Socket2)
    end)),
    send(Socket1, "newgame"),
    timer:sleep(infinity).

send(Socket, Message) ->
  ?PRINT(Message),
  gen_tcp:send(Socket, Message ++ "$").

loop(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {error, Reason} -> ?PRINT(Reason);
		{ok, Packet} ->
      scenario(Packet),
			loop(Socket)
	end.

director(S1, S2) ->
  receive
    {s2, connect, Id} ->
      send(S2, "connect;" ++ binary_to_list(Id))
  end.

scenario(<<"lobbyid;", Args/binary>>) ->
  Id = binary:part(Args, 0, 4),
  whereis(director) ! {s2, connect, Id};

scenario(Message) ->
  ?PRINT(Message).
