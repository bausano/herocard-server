%% hc_game provides process for handling a game.
%%
%% @author: Michael Bausano

-module(hc_game).

-include("../../shared.hrl").

-include("../../models/model_game.hrl").

-include("../../models/model_player.hrl").

-export([create/1, connect/1]).

create(Player) ->
  Game = #game {
    id = ?SOCKET_ID(Player#player.socket),
    players = {Player, null},
    move = 1,
    state = 1
  },
  whereis(lobbies) ! {add, self(), Game},
  receive
    {ok, Id} ->
      {ok, "lobyid;" ++ integer_to_list(Id)};
    error ->
      {error, "lobbyexists"}
  end.

connect(_Id) ->
  ok.
