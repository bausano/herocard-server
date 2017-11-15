%% hc_server provides TCP server for HeroCard game.
%%
%% @author: Michael Bausano

-module(hc_server).

-export([boot/1, state/0]).

%% Socket is type of port.
-type socket() :: port().

%% Boots TPC server on given port.
-spec boot(integer()) -> atom().

%% Returns jSON object with state of current game.
-spec state() -> atom().

%% Handles a request such as draw card, put card or attack card.
-spec request(socket(), binary()) -> tuple().

%% Name of the server.
-define(NAME, "HeroCard").

%% Server root directory.
-define(ROOT, "/home/michael/Code/erlang/herocard-server").

%% Server settings.
-define(OPTIONS, [
	binary,
	{packet, 0},
	{backlog, 15},
	{active, false},
	{reuseaddr, true}
]).

%% Defines a character every request sent to the server has to end with.
-define(DELIMINATOR, "$").

%% Debug function.
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

%% Booting the server. It's not parallel yet.
boot(Port) ->
	Pid = spawn_link(fun() ->
		%% Socket that's listening on given port.
		ListenSocket = gen_tcp:listen(Port, ?OPTIONS),
		%% Monitores socket connections.
		spawn(fun() -> acceptor(ListenSocket) end),
		timer:sleep(infinity)
	end),
	{ok, Pid}.

%% This process gets called everytime someone attemps to connect.
acceptor({ok, ListenSocket}) ->
	io:format("Opening connection \n", []),
	%% Handles connection and creates socket unique for that connection.
	{ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	%% Spawns worker for next connection.
	spawn(fun() -> acceptor({ok, ListenSocket}) end),
	%% TODO: Register socket.
	%% Monitores socket messages.
	loop(AcceptSocket, []);

acceptor({error, Reason}) -> ?PRINT(Reason).

%% Socket-communication loop. Messages sent are getting appended to data list.
loop(Socket, Data) ->
	inet:setopts(Socket, [{active, once}]),
	receive
		{tcp, Socket, Message} ->
			CurrentData = binary:list_to_bin([Data | [Message]]),
			case handle(Socket, CurrentData) of
				{ok, Response} ->
					gen_tcp:send(Socket, Response ++ "\n"),
					loop(Socket, []);
				{error, _Reason} ->
					gen_tcp:send(Socket, "error\n"),
					loop(Socket, []);
				_Default ->
					loop(Socket, CurrentData)
			end
	end.

%% Determines whener the message is a command or not.
%% Returns a tuple of 2.
handle(Socket, Message) ->
	Position = binary:match(Message, <<?DELIMINATOR>>),
	case Position of
		{EndPos, _} ->
			Command = binary:part(Message, 0, EndPos),
			request(Socket, Command);
		_Default -> {wait, nomatch}
	end.

request(Socket, <<"print", _/binary>>) ->
	{ok, "Here is some delicious message"};

request(Socket, <<"quit">>) ->
	ok = get_tcp:close(Socket),
	{ok, "closed"};

request(Socket, <<"newgame">>) ->
	{ok, "Starting new game and waiting for user to connect."};

request(Socket, <<"gamelist">>) ->
	{ok, "Returning gamelist."};

request(Socket, <<"connect;", Tail/binary>>) ->
	Args = get_arguments_list(Tail),
	?PRINT(Args),
	{ok, "Connecting to the game."};

request(_Socket, _Default) ->
	{error, nomatch}.

%% Parses binary function string into separate commands.
get_arguments_list(Binary) ->
	binary:split(Binary, <<";">>, [global]).

state() -> ok.
