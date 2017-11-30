-module(hc_arcade).

-include("../../models/model_game.hrl").

-export([boot/0]).

%% TODO: Arcade supervisor

boot() ->
  register(arcade, spawn(fun() ->
    arcade([])
  end)),

  register(lobbies, spawn(fun() ->
    lobbies(dict:new())
  end)).

arcade(Games) ->
  receive
    {add, Game} ->
      arcade([Game | Games])
  end.

lobbies(Lobbies) ->
  receive
    {add, From, Lobby} ->
      Lobby ! {id, self()},
      receive Val -> Id = Val end,
      case dict:is_key(Id, Lobbies) of
        true ->
          From ! error,
          lobbies(Lobbies);
        false ->
          From ! {ok, Id},
          lobbies(dict:append(Id, Lobby, Lobbies))
      end;

    {list, From} ->
      From ! Lobbies,
      lobbies(Lobbies)
  end.
