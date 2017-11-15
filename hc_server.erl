%% hc_server provides HTTP server for HeroCard game.
%%
%% @author: Michael Bausano

-module(hc_server).

-export([boot/1, state/0]).

%% Boots TPC server on given port.
-spec boot(integer()) -> atom().

%% Returns jSON object with state of current game.
-spec state() -> atom().

%% Handles a request such as draw card, put card or attack card.
-spec request(binary()) -> tuple().

%% Name of the server.
-define(NAME, "HeroCard").

%% Server root directory.
-define(ROOT, "/home/michael/Code/erlang/herocard-server").

%% Server settings.
-define(OPTIONS, [
	binary,
	{backlog, 15},
	{packet, 0},
	{active, false},
	{reuseaddr, true}
]).

%% Defines a character every request sent to the server has to start with.
-define(CMD_START, "$").

%% Defines a character every request sent to the server has to end with.
-define(CMD_STOP, "|").

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
			case handle(CurrentData) of
				{ok, Response} ->
					gen_tcp:send(Socket, Response),
					loop(Socket, []);
				{error, _Reason} ->
					gen_tcp:send(Socket, "error\n"),
					loop(Socket, []);
				_Default ->
					loop(Socket, CurrentData)
			end
	end.

handle(Message) ->
	Position = {
		binary:match(Message, <<?CMD_START>>),
		binary:match(Message, <<?CMD_STOP>>)
	},
	case Position of
		{{StartPos, StartLen}, {EndPos, EndLen}} when EndPos > StartPos ->
			Start = StartPos + StartLen,
			Command = binary:part(Message, Start, EndPos - Start),
			request(Command);
		_Default -> {wait, nomatch}
	end.

state() -> ok.

request(<<"print", _/binary>>) ->
	{ok, "Here is some delicious message \n"};

request(_Default) ->
	{error, nomatch}.
