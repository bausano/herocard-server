%% hc_server provides TCP server for HeroCard game.
%%
%% @author: Michael Bausano

-module(hc_server).

-include("../models/model_player.hrl").

-export([boot/1, respond/2]).

-import(hc_router, [request/3]).

-import(hc_arcade, [boot/0]).

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

%% Sends a message to client socket.
-spec respond(socket(), any()) -> atom().

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

%% Debug function.
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

%% Booting the server.
boot(Port) ->
  hc_arcade:boot(),
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
  %% Registers socket as a player.
  Player = hc_player:new(AcceptSocket),
	%% Monitores socket messages.
	loop(Player, []);

acceptor({error, _Reason}) ->
	%% TODO: Log error message
	error.

%% Socket-communication loop.
loop(Player, Data) ->
  Socket = Player#player.socket,
	%% Keeps socket in passive mode.
	inet:setopts(Socket, [{active, once}]),
	receive
		{tcp, Socket, Message} ->
			%% Apends packet to the list of packets.
			CurrentData = binary:list_to_bin(Data, Message]),
			case handle(Player, CurrentData) of
				%% If request was valid, sends a response to client.
				{ok, Response} ->
          ?PRINT(Response),
          respond(Socket, Response),
					loop(Player, []);

				{error, Reason} ->
					%% TODO: Log all error messages.
          respond(Socket, Reason),
					loop(Player, []);

				%% Breaking the loop.
				{stop, _} ->
          ?PRINT(closed),
          respond(Socket, "closed"),
					ok = gen_tcp:close(Socket);

				%% Fallback for other cases.
				_Default ->
					loop(Player, CurrentData)
			end
	end.

%% Determines whener the message is a command or not.
%% Returns a tuple of 2.
handle(Player, Message) ->
	Position = binary:match(Message, <<?DELIMITOR>>),
	case Position of
		{EndPos, _} ->
      ?PRINT(Message),
			Command = binary:part(Message, 0, EndPos),
			%% Parses binary function string into separate commands.
			[Route | Args] = binary:split(Command, <<";">>, [global]),
			%% Call router request.
			hc_router:request(Player, Route, Args);
		_Default -> {wait, nomatch}
	end.

%% Responds to client in standardized format.
respond(Socket, Response) ->
  Over = "\n" ++ ?DELIMITOR ++ "\n",
  ok = gen_tcp:send(Socket, Response ++ Over).
