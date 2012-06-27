-module(gpio).
-author('dusten@usa.net').

-export([init/3, release/1, start/2, start/3]).

%% gen_server callbacks
%%-behaviour(gen_server).
%%-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

GpioExport = "/sys/class/gpio/export",
GpioDebug  = "/sys/kernel/debug/omap_mux",
GpioGpio   = "/sys/class/gpio/gpio",
Ain	   = "/sys/devices/platform/omap/tsc",

-record(state, {pid,		% Process ID
		pin_fd,		% File Discripter for gpio pin
                pin_mode,	% Mode to set Pin to
		pin_direction,	% Using the Pin as Input or Output
		pin_gpio,	% GPIO number of the pin gpioX*32+[Y] 
		pin_mode0,	% Mode0 Name of pin for setting Mode and direction
		pin_proc,	% Proc value of pin
		pin_slew,	% slew rate - [fast|slow] 0=Fast 1=Slow
		pin_updown,	% Pin is a Pullup or Pulldown - [up|down] 0=pulldown 1=pullup
		pin_updown_e,	% Pin Pullup/Pulldown is enabled or disabled - [enabled|disabled] 0=enabled 1=disabled
                orig_state	% Mode of Pin before Changing
}).

start(Pin, Direction) ->
  State = gpio:init(Pin, 7, Direction, []),
  io:format(" Current State: ~p~n",[State]),
  State#state.pid = spawn(?MODULE, loop, State),
  State.

start(Pin, Mode, Direction) ->
  State = gpio:init(Pin, Mode, Direction, []),
  io:format(" Current State: ~p~n",[State]),
  State#state.pid = spawn(?MODULE, loop, State),
  State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%   Ext_Mode = [pin_updown,pin_updown_e,pin_slew]  %%
%%   Ext_Mode is a list that can be the following   %%
%% ["[up|down]","[enabled|disabled]","[fast|slow]"] %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(Pin, Mode, Direction, Ext_Mode) ->
  State = gpio:init(Pin, Mode, Direction, Ext_Mode),
  io:format(" Current State: ~p~n",[State]),
  State#state.pid = spawn(?MODULE, loop, State),
  State.

stop(State) ->
  State#state.pid ! stop.

init(Header, Mode, Direction, Ext_Mode) ->
  {Results, State} = case Header of
    {p8, 1}  ->  %% GND
      {error, {"Reserved PIN", "This PIN is used for GND"}};
    {p8, 2}  ->  %% GND
      {error, {"Reserved PIN", "This PIN is used for GND"}};
    {p8, 3}  ->  %% GPIO1_6    - R9  - MODE[0|1|7] 		- gpmc_ad6 
      case Mode of
%	0 ->;
%	1 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 38, pin_mode0 = "gpmc_ad6", pin_proc = "R9"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-3"}}
      end;
    {p8, 4}  ->  %% GPIO1_7    - T9  - MODE[0|1|7] 		- gpmc_ad7
      case Mode of
%	0 ->;
%	1 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 39, pin_mode0 = "gpmc_ad7", pin_proc = "T9"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-4"}}
      end;
    {p8, 5}  ->  %% GPIO1_2    - R8  - MODE[0|1|7] 		- gpmc_ad2
      case Mode of
%	0 ->;
%	1 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 34, pin_mode0 = "gpmc_ad2", pin_proc = "R8"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-5"}}
      end;
    {p8, 6}  ->  %% GPIO1_3    - T8  - MODE[0|1|7] 		- gpmc_ad3
      case Mode of
%	0 ->;
%	1 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 35, pin_mode0 = "gpmc_ad3", pin_proc = "T8"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-6"}}
      end;
    {p8, 7}  ->  %% Timer4     - R7  - MODE[0|4|7] 		- gpmc_advn_ale
      case Mode of
%	0 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 66, pin_mode0 = "gpmc_advn_ale", pin_proc ="R7"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-7"}}
      end;
    {p8, 8}  ->  %% Timer7     - T7  - MODE[0|4|7] 		- gpmc_oen_ren
      case Mode of
%	0 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 67, pin_mode0 = "gpmc_oen_ren", pin_proc = "T7"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-8"}}
      end;
    {p8, 9}  ->  %% Timer5     - T6  - MODE[0|4|7] 		- gpmc_ben0_cle
      case Mode of
%	0 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 69, pin_mode0 = "gpmc_ben0_cle", pin_proc = "T6"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-9"}}
      end;
    {p8, 10} ->  %% Timer6     - U6  - MODE[0|4|7] 		- gpmc_wen
      case Mode of
%	0 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 68, pin_mode0 = "gpmc_wen", pin_proc = "U6"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-10"}}
      end;
    {p8, 11} ->  %% GPIO1_13   - R12 - MODE[0|1|2|3|4|7] 	- gpmc_ad13
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 45, pin_mode0 = "gpmc_ad13", pin_proc = "R12"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-11"}}
      end;
    {p8, 12} ->  %% GPIO1_12   - T12 - MODE[0|1|2|3|4|7] 	- gpmc_ad12
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 44, pin_mode0 = "gpmc_ad12", pin_proc = "T12"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-12"}}
      end;
    {p8, 13} ->  %% EHRPWM2B   - T10 - MODE[0|1|2|3|4|7] 	- gpmc_ad9
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 23, pin_mode0 = "gpmc_ad9", pin_proc = "T10"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-13"}}
      end;
    {p8, 14} ->  %% GPIO0_26   - T11 - MODE[0|1|2|3|4|7] 	- gpmc_ad10
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 26, pin_mode0 = "gpmc_ad10", pin_proc = "T11"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-14"}}
      end;
    {p8, 15} ->  %% GPIO1_15   - U13 - MODE[0|1|2|3|4|7] 	- gpmc_ad15
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 47, pin_mode0 = "gpmc_ad15", pin_proc = "U13"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-15"}}
      end;
    {p8, 16} ->  %% GPIO1_14   - V13 - MODE[0|1|2|3|4|7] 	- gpmc_ad14
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 46, pin_mode0 = "gpmc_ad14", pin_proc = "V13"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-16"}}
      end;
    {p8, 17} ->  %% GPIO0_27   - U12 - MODE[0|1|2|3|4|7] 	- gpmc_ad11
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 27, pin_mode0 = "gpmc_ad11", pin_proc = "U12"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-17"}}
      end;
    {p8, 18} ->  %% GPIO2_1    - V12 - MODE[0|1|2|3|6|7] 	- gpmc_clk
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	6 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 65, pin_mode0 = "gpmc_clk", pin_proc = "V12"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-18"}}
      end;
    {p8, 19} ->  %% EHRPWM2A   - U10 - MODE[0|1|2|3|4|7] 	- gpmc_ad8
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 22, pin_mode0 = "gpmc_ad8", pin_proc = "U10"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-19"}}
      end;
    {p8, 20} ->  %% GPIO1_31   - V9  - MODE[0|1|2|7] 		- gpmc_csn2
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 63 , pin_mode0 = "gpmc_csn2", pin_proc = "V9"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-20"}}
      end;
    {p8, 21} ->  %% GPIO1_30   - U9  - MODE[0|1|2|7] 		- gpmc_csn1
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 62, pin_mode0 = "gpmc_csn1", pin_proc = "U9"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-21"}}
      end;
    {p8, 22} ->  %% GPIO1_5    - V8  - MODE[0|2|7] 		- gpmc_ad5
      case Mode of
%	0 ->;
%	2 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 37, pin_mode0 = "gpmc_ad5", pin_proc = "V8"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-22"}}
      end;
    {p8, 23} ->  %% GPIO1_4    - U8  - MODE[0|1|7] 		- gpmc_ad4
      case Mode of
%	0 ->;
%	1 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 36, pin_mode0 = "gpmc_ad4", pin_proc = "U8"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-23"}}
      end;
    {p8, 24} ->  %% GPIO1_1    - V7  - MODE[0|1|7] 		- gpmc_ad1
      case Mode of
%	0 ->;
%	1 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 33, pin_mode0 = "gpmc_ad1", pin_proc = "V7"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-24"}}
      end;
    {p8, 25} ->  %% GPIO1_0    - U7  - MODE[0|1|7] 		- gpmc_ad0
      case Mode of
%	0 ->;
%	1 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 32, pin_mode0 = "gpmc_ad0", pin_proc = "U7"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-25"}}
      end;
    {p8, 26} ->  %% GPIO1_29   - V6  - MODE[0|7] 		- gpmc_csn0
      case Mode of
%	0 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 61, pin_mode0 = "gpmc_csn0", pin_proc = "V6"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-26"}}
      end;
    {p8, 27} ->  %% GPIO1_22   - U5  - MODE[0|1|7] 		- lcd_vsync
      case Mode of
%	0 ->;
%	1 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 86, pin_mode0 = "lcd_vsync", pin_proc = "U5"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-27"}}
      end;
    {p8, 28} ->  %% GPIO1_24   - V5  - MODE[0|1|7] 		- lcd_pclk
      case Mode of
%	0 ->;
%	1 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 88, pin_mode0 = "lcd_pclk", pin_proc = "V5"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-28"}}
      end;
    {p8, 29} ->  %% GPIO1_23   - R5  - MODE[0|1|7] 		- lcd_hsync
      case Mode of
%	0 ->;
%	1 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 87, pin_mode0 = "lcd_hsync", pin_proc = "R5"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-29"}}
      end;
    {p8, 30} ->  %% GPIO1_25   - R6  - MODE[0|1|7] 		- lcd_ac_bias_en
      case Mode of
%	0 ->;
%	1 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 89, pin_mode0 = "lcd_ac_bias_en", pin_proc = "R6"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-30"}}
      end;
    {p8, 31} ->  %% UART5_CTSN - V4  - MODE[0|1|2|3|4|6|7] 	- lcd_data14
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
%	6 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 10, pin_mode0 = "lcd_data14", pin_proc = "V4"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-31"}}
      end;
    {p8, 32} ->  %% UART5_RTSN - T5  - MODE[0|1|2|3|4|6|7] 	- lcd_data15
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
%	6 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 11, pin_mode0 = "lcd_data15", pin_proc = "T5"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-32"}}
      end;
    {p8, 33} ->  %% UART4_RTSN - V3  - MODE[0|1|2|3|4|6|7] 	- lcd_data13
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
%	6 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 9, pin_mode0 = "lcd_data13", pin_proc = "V3"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-33"}}
      end;
    {p8, 34} ->  %% UART3_RTSN - U4  - MODE[0|1|2|3|4|6|7] 	- lcd_data11
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
%	6 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 81, pin_mode0 = "lcd_data11", pin_proc = "U4"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-34"}}
      end;
    {p8, 35} ->  %% UART4_CTSN - V2  - MODE[0|1|2|3|4|6|7] 	- lcd_data12
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
%	6 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 8, pin_mode0 = "lcd_data12", pin_proc = "V2"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-35"}}
      end;
    {p8, 36} ->  %% UART3_CTSN - U3  - MODE[0|1|2|3|6|7] 	- lcd_data10
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	6 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 80, pin_mode0 = "lcd_data10", pin_proc = "U3"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-36"}}
      end;
    {p8, 37} ->  %% UART5_TXD  - U1  - MODE[0|1|2|3|4|6|7] 	- lcd_data8
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
%	6 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 78, pin_mode0 = "lcd_data8", pin_proc = "U1"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-37"}}
      end;
    {p8, 38} ->  %% UART5_RXD  - U2  - MODE[0|1|2|3|4|6|7] 	- lcd_data9
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
%	6 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 79, pin_mode0 = "lcd_data9", pin_proc = "U2"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-38"}}
      end;
    {p8, 39} ->  %% GPIO2_12   - T3  - MODE[0|1|3|7] 		- lcd_data6
      case Mode of
%	0 ->;
%	1 ->;
%	3 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 76, pin_mode0 = "lcd_data6", pin_proc = "T3"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-39"}}
      end;
    {p8, 40} ->  %% GPIO2_13   - T4  - MODE[0|1|3|4|7] 		- lcd_data7
      case Mode of
%	0 ->;
%	1 ->;
%	3 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 77, pin_mode0 = "lcd_data7", pin_proc = "T4"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-40"}}
      end;
    {p8, 41} ->  %% GPIO2_10   - T1  - MODE[0|1|3|7] 		- lcd_data4
      case Mode of
%	0 ->;
%	1 ->;
%	3 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 74, pin_mode0 = "lcd_data4", pin_proc = "T1"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-41"}}
      end;
    {p8, 42} ->  %% GPIO2_11   - T2  - MODE[0|1|3|7] 		- lcd_data5
      case Mode of
%	0 ->;
%	1 ->;
%	3 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 75, pin_mode0 = "lcd_data5", pin_proc = "T2"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-42"}}
      end;
    {p8, 43} ->  %% GPIO2_8    - R3  - MODE[0|1|3|7] 		- lcd_data2
      case Mode of
%	0 ->;
%	1 ->;
%	3 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 72, pin_mode0 = "lcd_data2", pin_proc = "R3"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-43"}}
      end;
    {p8, 44} ->  %% GPIO2_9    - R4  - MODE[0|1|3|7] 		- lcd_data3
      case Mode of
%	0 ->;
%	1 ->;
%	3 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 73, pin_mode0 = "lcd_data3", pin_proc = "R4"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-44"}}
      end;
    {p8, 45} ->  %% GPIO2_6    - R1  - MODE[0|1|3|7] 		- lcd_data0
      case Mode of
%	0 ->;
%	1 ->;
%	3 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 70, pin_mode0 = "lcd_data0", pin_proc = "R1"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-45"}}
      end;
    {p8, 46} ->  %% GPIO2_7    - R2  - MODE[0|1|3|7] 		- lcd_data1
      case Mode of
%	0 ->;
%	1 ->;
%	3 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 71, pin_mode0 = "lcd_data1", pin_proc = "R2"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-46"}}
      end;
    {p9, 1}  ->  %% GND
      {error, {"Reserved PIN", "This PIN is used for GND"}};
    {p9, 2}  ->  %% GND
      {error, {"Reserved PIN", "This PIN is used for GND"}};
    {p9, 3}  ->  %% DC_3.3V
      {error, {"Reserved PIN", "This PIN is used for DC_3.3V"}};
    {p9, 4}  ->  %% DC_3.3V
      {error, {"Reserved PIN", "This PIN is used for DC_3.3V"}};
    {p9, 5}  ->  %% VDD_5V
      {error, {"Reserved PIN", "This PIN is used for VDD_5V"}};
    {p9, 6}  ->  %% VDD_5V
      {error, {"Reserved PIN", "This PIN is used for VDD_5V"}};
    {p9, 7}  ->  %% SYS_5V
      {error, {"Reserved PIN", "This PIN is used for SYS_5V"}};
    {p9, 8}  ->  %% SYS_5V
      {error, {"Reserved PIN", "This PIN is used for SYS_5V"}};
    {p9, 9}  ->  %% PWR_BUT
      {error, {"Reserved PIN", "This PIN is used for PWR_BUT"}};
    {p9, 10} ->  %% SYS_RESETn - A10 - MODE[0]			- REST_OUT
      {error, {"Reserved PIN", "This PIN is used for SYS_RESETn"}};
    {p9, 11} ->  %% UART4_RXD  - T17 - MODE[0|1|2|3|4|6|7]	- gpmc_wait0
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
%	6 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 30, pin_mode0 = "gpm_wait0", pin_proc = "T17"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p9-11"}}
      end;
    {p9, 12} ->  %% GPIO1_28   - U18 - MODE[0|1|2|3|4|6|7]	- gpmc_ben1	
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
%	6 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 60, pin_mode0 = "gpmc_ben1", pin_proc = "U18"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p9-12"}}
      end;
    {p9, 13} ->  %% UART4_TXD  - U17 - MODE[0|1|2|3|4|6|7]	- gpmc_wpn
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
%	6 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 31, pin_mode0 = "gpmc_wpn", pin_proc = "U17"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p9-13"}}
      end;
    {p9, 14} ->  %% EHRPWM1A   - U14 - MODE[0|1|2|3|4|6|7]	- gpmc_a2
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
%	6 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 50, pin_mode0 = "gpmc_a2", pin_proc = "U14"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p9-14"}}
      end;
    {p9, 15} ->  %% GPIO1_16   - R13 - MODE[0|1|2|3|4|6|7]	- gpmc_a0
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
%	6 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 48, pin_mode0 = "gpmc_a0", pin_proc = "R13"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p9-15"}}
      end;
    {p9, 16} ->  %% EHRPWM1B   - T14 - MODE[0|1|2|3|4|6|7]	- gpmc_a3
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
%	6 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 51, pin_mode0 = "gpmc_a3", pin_proc = "T14"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p9-16"}}
      end;
    {p9, 17} ->  %% I2C1_SCL   - A16 - MODE[0|1|2|3|7]		- spi0_cs0
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 5, pin_mode0 = "spi0_cs0", pin_proc = "A16"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p9-17"}}
      end;
    {p9, 18} ->  %% I2C1_SDA   - B16 - MODE[0|1|2|3|7]		- spi0_d1
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 4, pin_mode0 = "spi0_d1", pin_proc = "B16"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p9-18"}}
      end;
    {p9, 19} ->  %% I2C2_SCL   - D17 - MODE[0|1|2|3|4|7]	- uart1_rtsn
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 13, pin_mode0 = "uart1_rtsn", pin_proc = "D17"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p9-19"}}
      end;
    {p9, 20} ->  %% I2C2_SDA   - D18 - MODE[0|1|2|3|4|7]	- uart1_ctsn
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 12, pin_mode0 = "uart1_ctsn", pin_proc = "D18"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p9-20"}}
      end;
    {p9, 21} ->  %% UART2_TXD  - B17 - MODE[0|1|2|3|6|7]	- spi0_d0
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	6 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 3, pin_mode0 = "spi0_d0", pin_proc = "B17"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p9-21"}}
      end;
    {p9, 22} ->  %% UART2_RXD  - A17 - MODE[0|1|2|3|6|7]	- spi0_sclk
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
%	6 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 2, pin_mode0 = "spi0_sclk", pin_proc = "A17"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p9-22"}}
      end;
    {p9, 23} ->  %% GPIO1_17   - V14 - MODE[0|1|2|3|4|6|7]	- gpmc_a1
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
%	6 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 49, pin_mode0 = "gpmc_a1", pin_proc = "V14"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p9-23"}}
      end;
    {p9, 24} ->  %% UART1_TXD  - D15 - MODE[0|1|2|3|7]		- uart1_txd
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 15, pin_mode0 = "uart1_txd", pin_proc = "D15"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p9-24"}}
      end;
    {p9, 25} ->  %% GPIO3_21   - A14 - MODE[0|1|2|3|4|7]	- mcasp0_ahclkx
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 117, pin_mode0 = "mcasp0_ahclkx", pin_proc = "A14"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p9-25"}}
      end;
    {p9, 26} ->  %% UART1_RXD  - D16 - MODE[0|1|2|3|7]		- uart1_rxd
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 14, pin_mode0 = "uart1_rxd", pin_proc = "D16"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p9-26"}}
      end;
    {p9, 27} ->  %% GPIO3_19   - C13 - MODE[0|1|2|3|4|6|7]	- mcasp0_fsr
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
%	6 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 115, pin_mode0 = "mcasp0_fsr", pin_proc = "C13"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p9-27"}}
      end;
    {p9, 28} ->  %% SPI1_CS0   - C12 - MODE[0|1|2|3|4|7]	- mcasp0_ahclkr
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 113, pin_mode0 = "mcasp0_ahclkr", pin_proc = "C12"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p9-28"}}
      end;
    {p9, 29} ->  %% SPI1_D0    - B13 - MODE[0|1|3|4|7]		- mcasp0_fsx
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 111, pin_mode0 = "mcasp0_fsx", pin_proc = "B13"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p9-29"}}
      end;
    {p9, 30} ->  %% SPI1_D1    - D12 - MODE[0|1|3|4|7]		- mcasp0_axr0
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 112, pin_mode0 = "mcasp0_axr0", pin_proc = "D12"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p9-30"}}
      end;
    {p9, 31} ->  %% SPI1_SCLK  - A13 - MODE[0|1|3|4|7]		- mcasp0_aclkx
      case Mode of
%	0 ->;
%	1 ->;
%	3 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 110, pin_mode0 = "mcasp0_aclkx", pin_proc = "A13"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p9-31"}}
      end;
    {p9, 32} ->  %% VDD_ADC
      {error, {"Reserved PIN", "This PIN is used for VDD_ADC"}};
    {p9, 33} ->  %% AIN4       - C8  - MODE[]			- ain4
      {ok, #state{pin_mode = "0", pin_direction = "IN", pin_mode0 = "ain5" pin_proc = "C8"}};
    {p9, 34} ->  %% GNDA_ADC
      {error, {"Reserved PIN", "This PIN is used for GNDA_ADC"}};
    {p9, 35} ->  %% AIN6       - A5  - MODE[]			- ain6
      {ok, #state{pin_mode = "0", pin_direction = "IN", pin_mode0 = "ain7" pin_proc = "A5"}};
    {p9, 36} ->  %% AIN5       - A6  - MODE[]			- ain5
      {ok, #state{pin_mode = "0", pin_direction = "IN", pin_mode0 = "ain6" pin_proc = "A6"}};
    {p9, 37} ->  %% AIN2       - B7  - MODE[]			- ain2
      {ok, #state{pin_mode = "0", pin_direction = "IN", pin_mode0 = "ain3" pin_proc = "B7"}};
    {p9, 38} ->  %% AIN3       - A7  - MODE[]			- ain3
      {ok, #state{pin_mode = "0", pin_direction = "IN", pin_mode0 = "ain4" pin_proc = "A7"}};
    {p9, 39} ->  %% AIN0       - B6  - MODE[]			- ain0
      {ok, #state{pin_mode = "0", pin_direction = "IN", pin_mode0 = "ain1" pin_proc = "B6"}};
    {p9, 40} ->  %% AIN1       - C7  - MODE[]			- ain1
      {ok, #state{pin_mode = "0", pin_direction = "IN", pin_mode0 = "ain2" pin_proc = "C7"}};
    {p9, 41} ->  %% CLKOUT2    - D14 - MODE[0|2|3|4|6|7]	- xdma_event_intr1
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
%	6 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 20, pin_mode0 = "xdma_event_intr1", pin_proc = "D14"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p9-41"}}
      end;
    {p9, 42} ->  %% GPIO0_7    - C18 - MODE[0|1|2|3|4|6|7]	- ecap0_in_pwm0_out
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
%	6 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 7, pin_mode0 = "ecap0_in_pwm0_out", pin_proc = "C18"}};
	_ -> {error, {"Mode Error", "Invalide Mode for p8-20"}}
      end;
    {p9, 43} ->  %% GND
      {error, {"Reserved PIN", "This PIN is used for GND"}};
    {p9, 44} ->  %% GND
      {error, {"Reserved PIN", "This PIN is used for GND"}};
    {p9, 45} ->  %% GND
      {error, {"Reserved PIN", "This PIN is used for GND"}};
    {p9, 46} ->  %% GND
      {error, {"Reserved PIN", "This PIN is used for GND"}}
    end,

  State#state.orig_state = save_current_state(GpioDebug ++ State#.state.pin_gpio),
  <<Orig_Mode:3,Orig_Pin_UpDown_E:1,Orig_Pin_UpDown:1,Orig_Direction:1,Orig_Pin_Slew:1,_Reserved_1:9>> = <<State#state.orig_state:erlang:bit_size(State#state.orig_state)/integer-big>>;

  [V_1, V_2, V_3] = ext_mode(Ext_Mode),
 
  if V_1 == 2 -> State#state.pin_updown = binary:bin_to_list(Orig_Pin_UpDown);
    true -> State#state.pin_updown = V_1
  end,

  if V_2 == 2 -> State#state.pin_updown_e = binary:bin_to_list(Orig_Pin_UpDown_E);
    true -> State#state.pin_updown_e = V_2
  end,

  if V_3 == 2 -> State#state.pin_slew = binary:bin_to_list(Orig_Pin_Slew);
    true -> State#state.pin_slew = V3
  end,


  {ok, FdExport} = file:open(GpioExport, [write]),              
  file:write(FdExport, integer_to_list(State#state.pin_gpio)),
  file:close(FdExport),

  {ok, FdPinDir} = file:open(GpioGpio ++ integer_to_list(State#state.pin_gpio) ++ "/direction", [write]),              
  case State#state.pin_direction of
    in  -> file:write(FdPinDir, "in");
    out -> file:write(FdPinDir, "out")
  end,
  file:close(FdPinDir),

  {ok, State#state.pin_fd} = file:open(GpioGpio  ++ integer_to_list(State#state.pin_gpio) ++ "/value", [write]),             
  State.


release(State) ->
  {ok, FdUnexport} = file:open("/sys/class/gpio/unexport", [write]),
  file:write(FdUnexport, integer_to_list(State#state.pin_gpio)),
  file:close(FdUnexport).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 						   %%
%%          Internal Function Start Here           %%
%% 						   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


hex(N) when N < 10 -> $0+N;
hex(N) when N >= 10, N < 16 -> $a+(N-10).

to_hex(N) when N < 16 -> [hex(N rem 16)];
to_hex(N) when N >= 16, N < 256 -> [hex(N div 16), hex(N rem 16)].

hex_to_bin(L) ->
  binary:encode_unsigned(erlang:list_to_integer(L,16)).

bin_to_hex(L) ->
  B = binary:encode_unsigned(erlang:list_to_integer(L,2)),
  string:join([to_hex(X) || X <- erlang:bitstring_to_list(B)],"").


read_gpio(File) ->
  Results = case file:open(File, [read, binary, raw]) of
    {ok, Fd} ->
      Result = case file:pread(Fd, 0, 512) of
        eof ->
          {error, "Opended the file but reached EOF unexpetedly."};
%%          io:format(" Opended the file but reached EOF unexpetedly.~n"); %% Needs Change for Debugging
        {error, Res} ->
          Tmp = io_lib:format(" Error file:pread(~p) : ~p~n",[File,Res]),
          {error, Tmp};
        {ok, Res} ->
          {ok, Res}
      end,
      file:close(Fd),
      Result;
    {error, Res} ->
      Tmp = io_lib:format(" Error file:open(~p) : ~p~n",[File,Res]),
      {error, Tmp}
  end,
  Results.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%      Can use re:split in place of Token         %%
%% re:split(Raw_File,"[()]",[{return,list},trim]). %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

save_current_state(File) ->
  {ok,Raw_File} = read_gpio(File),
  [_Name,Reg,_Other] = string:tokens(binary:bin_to_list(Raw_File),"()"),
  [_Other_Reg,Cur_State] = string:tokens(Reg,erlang:atom_to_list(' = ')),
  [_,Cur] = string:tokens(Cur_State,"x"),          
  S = erlang:bit_size(erlang:list_to_binary(Cur)),
  <<Current_State:S/integer-little>>=erlang:list_to_binary(Cur). 

         pin_slew,       % slew rate - [fast|slow] 0=Fast 1=Slow
                pin_updown,     % Pin is a Pullup or Pulldown - [up|down] 0=pulldown 1=pullup
                pin_updown_e,   % Pin Pullup/Pulldown is enabled or disabled - [enabled|disabled] 0=enabled 1=disabled

ext_mode([]) -> ["2","2","2"];
ext_mode([Pin_UpDown]) ->
  if Pin_UpDown == 0 ; Pin_UpDown == 1 -> string:join([erlang:integer_to_list(Pin_UpDown),"2","2"],"");  
    true -> {error,"Invalid Ext_Mode Value Pin_UpDown"}
  end;
ext_mode([Pin_UpDown,Pin_UpDown_E]) ->
  if Pin_UpDown == 0 ; Pin_UpDown == 1,
     Pin_UpDown_E == 0 ; Pin_UpDown_E == 1 -> string:join([erlang:integer_to_list(Pin_UpDown),erlang:integer_to_list(Pin_UpDown_E),"2"],"");  
    true -> {error,"Invalid Ext_Mode Value Pin_UpDown_E or Pin_UpDown"}
  end;
ext_mode([Pin_UpDown, Pin_UpDown_E,Pin_Slew]) ->
  if Pin_Slew == 0 ; Pin_Slew == 1,
     Pin_UpDown == 0 ; Pin_UpDown == 1,
     Pin_UpDown_E == 0 ; Pin_UpDown_E == 1 -> string:join([erlang:integer_to_list(Pin_UpDown),erlang:integer_to_list(Pin_UpDown_E),erlang:integer_to_list(Pin_Slew)],"");  
    true -> {error,"Invalid Ext_Mode Value Pin_Slew or Pin_UpDown or Pin_UpDown_E"}
  end.


ext_mode([Pin_UpDown]) ->
  if Pin_UpDown == 0 ; Pin_UpDown == 1 -> string:join([erlang:integer_to_list(Pin_UpDown),"2","2"],"");  
    true -> {error,"Invalid Ext_Mode Value Pin_UpDown"}
  end;
ext_mode([Pin_UpDown,Pin_UpDown_E]) ->
  if Pin_UpDown == 0 ; Pin_UpDown == 1,
     Pin_UpDown_E == 0 ; Pin_UpDown_E == 1 -> string:join([erlang:integer_to_list(Pin_UpDown),erlang:integer_to_list(Pin_UpDown_E),"2"],"");  
    true -> {error,"Invalid Ext_Mode Value Pin_UpDown_E or Pin_UpDown"}
  end;
ext_mode([Pin_UpDown, Pin_UpDown_E,Pin_Slew]) ->
  if Pin_Slew == 0 ; Pin_Slew == 1,
     Pin_UpDown == 0 ; Pin_UpDown == 1,
     Pin_UpDown_E == 0 ; Pin_UpDown_E == 1 -> string:join([erlang:integer_to_list(Pin_UpDown),erlang:integer_to_list(Pin_UpDown_E),erlang:integer_to_list(Pin_Slew)],"");  
    true -> {error,"Invalid Ext_Mode Value Pin_Slew or Pin_UpDown or Pin_UpDown_E"}
  end.




%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
