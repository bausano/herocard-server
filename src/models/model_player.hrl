%% Record of a player stores game info.
-record(player, {
  %% Id of player matches id of socket.
  id,
  %% AcceptSocket is directly bound to a player.
  socket,
  %% Game pid or null if not in game.
  game = null
}).
