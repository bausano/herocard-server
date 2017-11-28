%% hc_player provides interface for working with players.
%%
%% @author: Michael Bausano

-module(hc_player).

-include("../../shared.hrl").

-include("../../models/model_player.hrl").

-export([new/1]).

new(Socket) ->
  Id = ?SOCKET_ID(Socket),
  #player{id=Id, socket=Socket}.
