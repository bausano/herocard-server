-module(hc_server_spec).

-include_lib("eunit/include/eunit.hrl").

-import(e2e_testers, [gen/1, action/2, expect/2]).

%% Debug function.
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

run_test_() ->
  e2e_testers:gen([
    host, guest, git
  ]),
  {inorder, [
    {"put 42",	?_test(undefined = put(foo, 42))},
    {"get 42",	?_test(42 = get(foo))}
  ]},
  connection_scenario(),
  invalid_behavior_scenario().

connection_scenario() ->
  {ok, GameId} = e2e_testers:action(host, newgame),
  {ok, _List} = e2e_testers:action(guest, gamelist),
  ok = e2e_testers:action(guest, {connect, GameId}),
  ok = e2e_testers:expect(host, [<<"connection;">>, {host, id}]).

invalid_behavior_scenario() ->
  {ok, GitGameId} = e2e_testers:action(git, newgame),
  error = e2e_testers:action(git, newgame),
  error = e2e_testers:action(git, {connect, GitGameId}).
