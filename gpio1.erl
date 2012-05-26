-module(gpio1).
-author('dusten@usa.net').

-export([init/3, release/1]).	%, current/1, last/1]).

%% gen_server callbacks
%%-behaviour(gen_server).
%%-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {pin_fd,		% File Discripter for gpio pin writing
                pin_mode,	% Mode to set Pin to
		pin_direction,	% Using the Pin as Input or Output
		pin_gpio,	% GPIO number of the pin gpioX*32+[Y] 
		pin_mode0,	% Mode0 Name of pin for setting Mode and direction
		pin_proc,	% Proc value of pin
                orig_mode	% Mode of Pin before Changing
}).

init(Header, Mode, Direction) ->

  {Results, State} = case Header of
    {p8, 1}  ->  %% GND
      {error, {"Reserved PIN", "This PIN is used for GND"}};
    {p8, 2}  ->  %% GND
      {error, {"Reserved PIN", "This PIN is used for GND"}};
    {p8, 3}  ->  %% GPIO1_6    - R9  - MODE[0|1|7] 		- gpmc_ad6 
      case Mode of
%        0 ->;
%	1 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 38, pin_mode0 = "gpmc_ad6", pin_proc = "R9"}};
	_ -> {error, {"Mode Error", "Invalide Mode for P8-3"}}
      end;
    {p8, 4}  ->  %% GPIO1_7    - T9  - MODE[0|1|7] 		- gpmc_ad7
      case Mode of
%        0 ->;
%	1 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 39, pin_mode0 = "gpmc_ad7", pin_proc = "T9"}};
	_ -> {error, {"Mode Error", "Invalide Mode for P8-4"}}
      end;
    {p8, 5}  ->  %% GPIO1_2    - R8  - MODE[0|1|7] 		- gpmc_ad2
      case Mode of
%        0 ->;
%	1 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 34, pin_mode0 = "gpmc_ad2", pin_proc = "R8"}};
	_ -> {error, {"Mode Error", "Invalide Mode for P8-5"}}
      end
    end,

  io:format(" Current State: ~p~n",[State]),

  {ok, FdExport} = file:open("/sys/class/gpio/export", [write]),

  file:write(FdExport, integer_to_list(State#state.pin_gpio)),
  file:close(FdExport),

  {ok, FdPinDir} = file:open("/sys/class/gpio/gpio" ++ integer_to_list(State#state.pin_gpio) ++ "/direction", [write]),
  case Direction of
    in  -> file:write(FdPinDir, "in");
    out -> file:write(FdPinDir, "out")
  end,
  file:close(FdPinDir),

  {ok, FdPinVal} = file:open("/sys/class/gpio/gpio" ++ integer_to_list(State#state.pin_gpio) ++ "/value", [write]),
  FdPinVal.

release(State) ->
  {ok, FdUnexport} = file:open("/sys/class/gpio/unexport", [write]),
  file:write(FdUnexport, integer_to_list(State#state.pin_gpio)),
  file:close(FdUnexport).


%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

