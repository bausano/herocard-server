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
  Pid = spawn(fun() -> loop(Game) end),
  whereis(lobbies) ! {add, self(), Pid},
  receive
    {ok, Id} ->
      {ok, "lobbyid;" ++ integer_to_list(Id)};
    error ->
      {error, "lobbyexists"}
  end.

loop(Game) ->
  receive
    {id, From} ->
      From ! Game#game.id,
      loop(Game);

    {players, From} ->
      From ! Game#game.players,
      loop(Game);

    {connect_player, From, Player} ->
      {_, Player2} = Game#game.players,
      case Player2 of
        null ->
           %% TODO copy game w/ player.
          From ! {ok, "xdd"},
          loop(Game);
        _Default ->
          From ! {error, "playerexists"},
          loop(Game)
      end
  end.

connect(_Id) ->
  ok.
