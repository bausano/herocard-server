%% hc_server provides TCP server for HeroCard game.
%%
%% @author: Michael Bausano

-module(hc_server).

-export([boot/1]).

-import(hc_router, [request/3]).

%% Socket is type of port.
-type socket() :: port().

%% Boots TPC server on given port.
-spec boot(integer()) -> tuple().

%% Spawns new listen process.
-spec acceptor(tuple()) -> any().

%% Listens to messages from socket.
-spec loop(socket(), binary()) -> any().

%% Handles a message sent by client.
-spec handle(socket(), binary()) -> tuple().

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
-define(DELIMITOR, "$").

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
	%% Handles connection and creates socket unique for that connection.
	{ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	%% Spawns worker for next connection.
	spawn(fun() -> acceptor({ok, ListenSocket}) end),
	%% Monitores socket messages.
	loop(AcceptSocket, []);

acceptor({error, _Reason}) ->
	%% TODO: Log error message
	error.

%% Socket-communication loop.
loop(Socket, Data) ->
	%% Keeps socket in passive mode.
	inet:setopts(Socket, [{active, once}]),
	receive
		{tcp, Socket, Message} ->
			%% Apends packet to the list of packets.
			CurrentData = binary:list_to_bin([Data | [Message]]),
			case handle(Socket, CurrentData) of
				%% If request was valid, sends a response to client.
				{ok, Response} ->
					ok = gen_tcp:send(Socket, Response ++ "\n"),
					loop(Socket, []);

				{error, Reason} ->
					%% TODO: Log all error messages.
					ok = gen_tcp:send(Socket, Reason ++ "\n"),
					loop(Socket, []);

				%% Breaking the loop.
				{stop, _} ->
					ok = gen_tcp:send(Socket, "closed\n"),
					ok = gen_tcp:close(Socket);

				%% Fallback for other cases.
				_Default ->
					loop(Socket, CurrentData)
			end
	end.

%% Determines whener the message is a command or not.
%% Returns a tuple of 2.
handle(Socket, Message) ->
	Position = binary:match(Message, <<?DELIMITOR>>),
	case Position of
		{EndPos, _} ->
			Command = binary:part(Message, 0, EndPos),
			%% Parses binary function string into separate commands.
			[Route | Args] = binary:split(Command, <<";">>, [global]),
			%% Call router request.
			hc_router:request(Socket, Route, Args);
		_Default -> {wait, nomatch}
	end.
