%% hc_player provides interface for working with players.
%%
%% @author: Michael Bausano

-module(hc_player).

-include("../../shared.hrl").

-include("../../models/model_player.hrl").

-export([new/1]).

%% Creates a new player struct.
new(Socket) ->
  %% Id of a player is same as the ID of Socket that
  %% the player first connected to the server with.
  Id = ?SOCKET_ID(Socket),
  #player{id=Id, socket=Socket}.
