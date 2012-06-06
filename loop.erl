-module(loop).
-export([loop/2]).

loop(Pin, State) ->
  receive
    on -> 
      file:write(State#state.pin_fd, "1"),
      loop(Pin, State);
    off ->
      file:write(State#state.pin_fd, "0"),
      loop(Pin, State);
    {blink, Delay} ->
      file:write(State#state.pin_fd, "1"),
      timer:sleep(Delay),
      file:write(State#state.pin_fd, "0"),
      timer:sleep(Delay),
      self() ! {blink, Delay},
      loop(Pin, State);
    stop ->
      file:close(State#state.pin_fd),
      gpio:release(State),
      ok
  end.
