-module(hc_server_spec).

-include_lib("eunit/include/eunit.hrl").

-import(hc_game, [create/1]).

length_test() -> ?assert(3 =:= 3).

pls_test() -> ?assertEqual(ok, hc_game:create("")).
