%% Record of game stores a game info.
-record(game, {
  %% String id of game under which it's registered.
  id,
  %% Current AcceptSocket. Might need revision later on.
  socket,
  %% Tuple of two players.
  players,
  %% Id of player that is currently on move.
  move,
  %% JSON object storing game state.
  state = 1
}).
