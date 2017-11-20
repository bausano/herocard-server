%% hc_router routes the commands that server receives and returns a message
%% that is sent to client.
%%
%% @author: Michael Bausano

-module(hc_router).

-export([request/3]).

-import(hc_game, [create/1, fetch/1]).

%% Socket is type of port.
-type socket() :: port().

%% Routing method.
%% @param socket() Client socket that request has been sent from.
%% @param binary() Route name.
%% @param list() Arguments.
%%
%% @return tuple() as
%%	{ok, string_message()}
%%	{error, atom_reason()}
-spec request(socket(), binary(), list()) -> tuple().

%% Debug function.
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

request(Socket, <<"print">>, _) ->
	{ok, "Here is some delicious message"};

request(_Socket, <<"quit">>, _) ->
	{stop, byclient};

request(Socket, <<"newgame">>, _) ->
	{ok, "Starting new game and waiting for user to connect."};

request(Socket, <<"gamelist">>, _) ->
	{ok, "Returning gamelist."};

request(Socket, <<"connect">>, Args) ->
	{ok, "Connecting to the game."};

request(_Socket, _Default, _Args) ->
	{error, "Invalid command."}.
