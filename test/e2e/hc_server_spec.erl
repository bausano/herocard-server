-module(hc_server_spec).

-include_lib("eunit/include/eunit.hrl").

-import(e2e_testers, [gen/1, action/2, expect/1]).

spawn_testers_test() ->
  Spawned = e2e_testers:gen([host, guest, git]),
  ?assertEqual(Spawned, ok).

it_starts_a_new_game_test() ->
  Lobby = e2e_testers:action(host, <<"newgame">>),
  ?assertMatch(<<"lobbyid;", _GameId/binary>>, Lobby).

it_connects_player_to_a_game_test() ->
  GameList = e2e_testers:action(guest, <<"gamelist">>),
  ?assertMatch(<<"gameids;", _GameId/binary>>, GameList),
  <<"gameids;", GameId/binary>> = GameList,
  Connecting = e2e_testers:action(guest, binary:list_to_bin([<<"connect;">>, GameId])),
  ?assertEqual(<<"connected">>, Connecting).

it_prevents_player_to_connect_to_his_own_game_test() ->
  Lobby = e2e_testers:action(git, "newgame"),
  <<"lobbyid;", GameId/binary>> = Lobby,
  AnotherLobby = e2e_testers:action(git, "newgame"),
  ConnectingWhileInGame = e2e_testers:action(git, binary:list_to_bin([<<"connect;">>, GameId])),
  %% TODO: Move error messages to a shared file.
  ?assertEqual(<<"error;You have already started a game!">>, AnotherLobby),
  ?assertEqual(<<"error;You are already in a game.">>, ConnectingWhileInGame).
