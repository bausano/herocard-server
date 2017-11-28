%% Debug function.
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n",[?MODULE, ?LINE, ??Var, Var])).

-define(SOCKET_ID(Socket), proplists:get_value(id, erlang:port_info(Socket))).
