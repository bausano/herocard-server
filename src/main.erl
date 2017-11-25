-module(main).

-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

-export([start/0]).

-import(hc_server, [boot/1]).

start() ->
  {ok, _Pid} = hc_server:boot(5551),
  timer:sleep(infinity).
