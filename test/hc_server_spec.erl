-module(hc_server_spec).

-include_lib("eunit/include/eunit.hrl").

-import(e2e_testers, [gen/1, action/2, expect/1]).

%% Debug function.
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

run_test() ->
  e2e_testers:gen([
    host, guest, git
  ]),
  connection_scenario().
  %%invalid_behavior_scenario().

connection_scenario() ->
  Lobby = e2e_testers:action(host, <<"newgame">>),
  ?assertMatch(<<"lobbyid;", _GameId/binary>>, Lobby),
  <<"lobbyid;", GameId/binary>> = Lobby,
  ?assertEqual(<<"3000">>, GameId),
  %%Message = e2e_testers:action(host, "connect"),
  Name = e2e_testers:action(guest, <<"auth">>),
  ?assertEqual(<<"Michael">>, Name).

  %%?assertEqual(Name, <<"Michael">>).
  %%?assertMatch(
  %%  {ok, _},
  %%  e2e_testers:action(host, "newgame")
  %%).
  %%List = e2e_testers:action(guest, "gamelist"),
  %%?PRINT("List"),
  %%?PRINT(List),
  %%Success = e2e_testers:action(guest, "connect;" ++ GameId),
  %%?PRINT("Success"),
  %%?PRINT(Success),
  %%ConnectMessage = e2e_testers:expect(host),
  %%?PRINT("ConnectMessage"),
  %%?PRINT(ConnectMessage).

invalid_behavior_scenario() ->
  {ok, GitGameId} = e2e_testers:action(git, "newgame"),
  error = e2e_testers:action(git, "newgame"),
  error = e2e_testers:action(git, "connect;" ++ GitGameId).
