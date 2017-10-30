%% hc_server provides HTTP server for HeroCard game.
%%
%% @author: Michael Bausano

-module(hc_server).

-export([boot/2, state/0, request/0]).

%% Boots server on given port and with given host (IP, "localhost", ...).
-spec boot(integer(), any()) -> atom().

%% Returns jSON object with state of current game.
-spec state() -> atom().

%% Handles a request such as draw card, put card or attack card.
-spec request() -> atom().

%% Name of the server.
-define(NAME, "HeroCard").

%% Server root directory.
-define(ROOT, "/home/michael/Code/erlang/herocard-server").

%% Debug function.
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

boot(Port, Host) ->
	serve(inets:starts(), Port, Host),
	ok.

serve(ok, Port, Host) ->
	Server = inets:start(
	  httpd,
	  [
	   { port, Port },
	   { server_name, ?NAME },
	   { server_root, ?ROOT },
	   { document_root, ?ROOT ++ "/www" },
	   { bind_address, Host }
	  ]
	 ),
	?PRINT(Server).

state() -> ok.

request() -> ok.
