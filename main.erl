-module(main).

-export([start/0]).

-import(hc_server, [boot/2]).

-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

start() ->
	hc_server:boot(0, "localhost").

