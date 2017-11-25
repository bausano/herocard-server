%% hc_game provides process for handling a game.
%%
%% @author: Michael Bausano

-module(hc_game).

-export([fetch/1, create/1]).

%% Record of game stores a game info.
-record(game, {
  %% String id of game under which it's registered.
  id,
  %% Current AcceptSocket. Might need revision later on.
  socket,
  %% Tuple of two players.
  players,
  %% Id of player that is currently on move.
  move,
  %% JSON object storing game state.
  state
}).

create(Data) ->
  ok.

fetch(Id) ->
  ok.
