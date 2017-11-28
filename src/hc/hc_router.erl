%% hc_router routes the commands that server receives and returns a message
%% that is sent to client.
%%
%% @author: Michael Bausano

-module(hc_router).

-export([request/3]).

-import(hc_game, [create/1]).

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

request(_Player, <<"quit">>, _) ->
	{stop, byclient};

request(Player, <<"newgame">>, _) ->
  hc_game:create(Player);

request(_Player, <<"gamelist">>, _) ->
  whereis(lobbies) ! {list, self()},
  receive
    Dict -> {ok, dict:fold(fun(Id, _Lobby, AccIn) ->
      AccIn ++ ";" ++ integer_to_list(Id)
    end, "gameids", Dict)}
  end;

request(_Player, <<"connect">>, Args) ->
  ?PRINT(Args),
	{ok, "Connecting to the game as "};

request(Player, <<"auth">>, _Args) ->
  ?PRINT(Player),
	{ok, "Michael"};

request(_Player, _Default, _Args) ->
	{error, "Invalid command."}.
