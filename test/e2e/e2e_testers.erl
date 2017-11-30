-module(e2e_testers).

-export([gen/1, action/2, expect/2]).

-define(PORT, 5551).

-define(ADDR, {127,0,0,1}).

-define(OPTIONS, [binary, {active, false}]).

%% Debug function.
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

gen(Testers) ->
  Pid = spawn(fun() ->
    scenarios(spawn_all_testers(Testers))
  end),
  register(testers, Pid).

scenarios(Testers) ->
  receive
    {From, Tester, newgame} ->
      Pid = locate_tester(Testers, Tester),
      Pid ! {self(), send, "newgame"},
      receive Response -> From ! Response end;
    {From, Tester, gamelist} ->
      Pid = locate_tester(Testers, Tester),
      Pid ! {self(), send, "gamelist"},
      receive Response -> From ! Response end;
    {From, Tester, {connect, GameId}} ->
      Pid = locate_tester(Testers, Tester),
      Pid ! {self(), send, "connect;" ++ GameId},
      receive Response -> From ! Response end;
    {_From, _Tester, {expect, Pattern}} ->
      ?PRINT(Pattern)
  end.

action(Tester, Action) ->
  whereis(testers) ! {self(), Tester, Action},
  receive
    Response -> Response
  end.

expect(Tester, Pattern) ->
  action(Tester, {expect, Pattern}).

locate_tester([], _Tester) -> error;

locate_tester([{Tester, Pid} | _Tail], Tester) -> Pid;

locate_tester([_Head | Tail], Tester) ->
  locate_tester(Tail, Tester).

spawn_all_testers([]) -> [];

spawn_all_testers([Tester | Tail]) ->
  [{Tester, spawn_tester()} | spawn_all_testers(Tail)].

spawn_tester() ->
  {ok, Socket} =  gen_tcp:connect(?ADDR, ?PORT, ?OPTIONS),
  spawn(fun() -> listen_loop(Socket, "") end).

listen_loop(Socket, LastMessage) ->
  receive
    {From, lastmessage} ->
      From ! LastMessage,
      listen_loop(Socket, LastMessage);
    {From, send, Message} ->
      From ! gen_tcp:send(Socket, Message ++ "$"),
      listen_loop(Socket, LastMessage)
  end,
  case gen_tcp:recv(Socket, 0) of
    {error, Reason} -> ?PRINT(Reason);
    {ok, Packet} ->
      listen_loop(Socket, Packet)
  end.
