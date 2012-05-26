-module(gpio).
-author('dusten@usa.net').

-export([init/3, release/1]).

%% gen_server callbacks
%%-behaviour(gen_server).
%%-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {pid,		% PID for looping
		pin_fd,		% File Discripter for gpio pin
                pin_mode,	% Mode to set Pin to
		pin_direction,	% Using the Pin as Input or Output
		pin_gpio,	% GPIO number of the pin gpioX*32+[Y] 
		pin_mode0,	% Mode0 Name of pin for setting Mode and direction
		pin_proc,	% Proc value of pin
                orig_mode	% Mode of Pin before Changing
}).

start(Pin, Direction) ->
  State = gpio:init(Pin, 7, Direction),
  State#state.pid = spawn(?MODULE, loop, State),
  State.

stop(State) ->
  State#state.pid ! stop.

init(Header, Mode, Direction) ->
  {Results, State} = case Header of
    {P8, 1}  ->  %% GND
      {error, {"Reserved PIN", "This PIN is used for GND"}};
    {P8, 2}  ->  %% GND
      {error, {"Reserved PIN", "This PIN is used for GND"}};
    {P8, 3}  ->  %% GPIO1_6    - R9  - MODE[0|1|7] 		- gpmc_ad6 
      case Mode of
%	0 ->;
%	1 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 38, pin_mode0 = "gpmc_ad6", pin_proc = "R9"}};
	_ -> {error, {"Mode Error", "Invalide Mode for P8-3"}}
      end;
    {P8, 4}  ->  %% GPIO1_7    - T9  - MODE[0|1|7] 		- gpmc_ad7
      case Mode of
%	0 ->;
%	1 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 39, pin_mode0 = "gpmc_ad7", pin_proc = "T9"}};
	_ -> {error, {"Mode Error", "Invalide Mode for P8-4"}}
      end;
    {P8, 5}  ->  %% GPIO1_2    - R8  - MODE[0|1|7] 		- gpmc_ad2
      case Mode of
%	0 ->;
%	1 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 34, pin_mode0 = "gpmc_ad2", pin_proc = "R8"}};
	_ -> {error, {"Mode Error", "Invalide Mode for P8-5"}}
      end;
    {P8, 6}  ->  %% GPIO1_3    - T8  - MODE[0|1|7] 		- gpmc_ad3
      case Mode of
%	0 ->;
%	1 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 35, pin_mode0 = "gpmc_ad3", pin_proc = "T8"}};
	_ -> {error, {"Mode Error", "Invalide Mode for P8-6"}}
      end;
    {P8, 7}  ->  %% Timer4     - R7  - MODE[0|4|7] 		- gpmc_advn_ale
      case Mode of
%	0 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 66, pin_mode0 = "gpmc_advn_ale", pin_proc ="R7"}};
	_ -> {error, {"Mode Error", "Invalide Mode for P8-7"}}
      end;
    {P8, 8}  ->  %% Timer7     - T7  - MODE[0|4|7] 		- gpmc_oen_ren
      case Mode of
%	0 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 67, pin_mode0 = "gpmc_oen_ren", pin_proc = "T7"}};
	_ -> {error, {"Mode Error", "Invalide Mode for P8-8"}}
      end;
    {P8, 9}  ->  %% Timer5     - T6  - MODE[0|4|7] 		- gpmc_ben0_cle
      case Mode of
%	0 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 69, pin_mode0 = "gpmc_ben0_cle", pin_proc = "T6"}};
	_ -> {error, {"Mode Error", "Invalide Mode for P8-9"}}
      end;
    {P8, 10} ->  %% Timer6     - U6  - MODE[0|4|7] 		- gpmc_wen
      case Mode of
%	0 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 68, pin_mode0 = "gpmc_wen", pin_proc = "U6"}};
	_ -> {error, {"Mode Error", "Invalide Mode for P8-10"}}
      end;
    {P8, 11} ->  %% GPIO1_13   - R12 - MODE[0|1|2|3|4|7] 	- gpmc_ad13
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 45, pin_mode0 = "gpmc_ad13", pin_proc = "R12"}};
	_ -> {error, {"Mode Error", "Invalide Mode for P8-11"}}
      end;
    {P8, 12} ->  %% GPIO1_12   - T12 - MODE[0|1|2|3|4|7] 	- gpmc_ad12
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = 44, pin_mode0 = "gpmc_ad12", pin_proc = "T12"}};
	_ -> {error, {"Mode Error", "Invalide Mode for P8-12"}}
      end;
    {P8, 13} ->  %% EHRPWM2B   - T10 - MODE[0|1|2|3|4|7] 	- gpmc_ad9
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = , pin_mode0 = "gpmc_ad9", pin_proc = "T10"}};
	_ -> {error, {"Mode Error", "Invalide Mode for P8-13"}}
      end;
    {P8, 14} ->  %% GPIO0_26   - T11 - MODE[0|1|2|3|4|7] 	- gpmc_ad10
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = , pin_mode0 = "", pin_proc = ""}};
	_ -> {error, {"Mode Error", "Invalide Mode for P8-14"}}
      end;
    {P8, 15} ->  %% GPIO1_15   - U13 - MODE[0|1|2|3|4|7] 	- gpmc_ad15
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = , pin_mode0 = "", pin_proc = ""}};
	_ -> {error, {"Mode Error", "Invalide Mode for P8-15"}}
      end;
    {P8, 16} ->  %% GPIO1_14   - V13 - MODE[0|1|2|3|4|7] 	- gpmc_ad14
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = , pin_mode0 = "", pin_proc = ""}};
	_ -> {error, {"Mode Error", "Invalide Mode for P8-16"}}
      end;
    {P8, 17} ->  %% GPIO0_27   - U12 - MODE[0|1|2|3|4|7] 	- gpmc_ad11
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = , pin_mode0 = "", pin_proc = ""}};
	_ -> {error, {"Mode Error", "Invalide Mode for P8-17"}}
      end;
    {P8, 18} ->  %% GPIO2_1    - V12 - MODE[0|1|2|3|6|7] 	- gpmc_clk
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	6 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = , pin_mode0 = "", pin_proc = ""}};
	_ -> {error, {"Mode Error", "Invalide Mode for P8-18"}}
      end;
    {P8, 19} ->  %% EHRPWM2A   - U10 - MODE[0|1|2|3|4|7] 	- gpmc_ad8
      case Mode of
%	0 ->;
%	1 ->;
%	2 ->;
%	3 ->;
%	4 ->;
	7 -> {ok, #state{pin_mode = Mode, pin_direction = Direction, pin_gpio = , pin_mode0 = "", pin_proc = ""}};
	_ -> {error, {"Mode Error", "Invalide Mode for P8-19"}}
      end;
    {P8, 20} ->  %% GPIO1_31   - V9  - MODE[0|1|2|7] 		- gpmc_csn2
    {P8, 21} ->  %% GPIO1_30   - U9  - MODE[0|1|2|7] 		- gpmc_csn1
    {P8, 22} ->  %% GPIO1_5    - V8  - MODE[0|2|7] 		- gpmc_ad5
    {P8, 23} ->  %% GPIO1_4    - U8  - MODE[0|1|7] 		- gpmc_ad4
    {P8, 24} ->  %% GPIO1_1    - V7  - MODE[0|1|7] 		- gpmc_ad1
    {P8, 25} ->  %% GPIO1_0    - U7  - MODE[0|1|7] 		- gpmc_ad0
    {P8, 26} ->  %% GPIO1_29   - V6  - MODE[0|7] 		- gpmc_csn0
    {P8, 27} ->  %% GPIO1_22   - U5  - MODE[0|1|7] 		- lcd_vsync
    {P8, 28} ->  %% GPIO1_24   - V5  - MODE[0|1|7] 		- lcd_pclk
    {P8, 29} ->  %% GPIO1_23   - R5  - MODE[0|1|7] 		- lcd_hsync
    {P8, 30} ->  %% GPIO1_25   - R6  - MODE[0|1|7] 		- lcd_ac_bias_en
    {P8, 31} ->  %% UART5_CTSN - V4  - MODE[0|1|2|3|4|6|7] 	- lcd_data14
    {P8, 32} ->  %% UART5_RTSN - T5  - MODE[0|1|2|3|4|6|7] 	- lcd_data15
    {P8, 33} ->  %% UART4_RTSN - V3  - MODE[0|1|2|3|4|6|7] 	- lcd_data13
    {P8, 34} ->  %% UART3_RTSN - U4  - MODE[0|1|2|3|4|6|7] 	- lcd_data11
    {P8, 35} ->  %% UART4_CTSN - V2  - MODE[0|1|2|3|4|6|7] 	- lcd_data12
    {P8, 36} ->  %% UART3_CTSN - U3  - MODE[0|1|2|3|6|7] 	- lcd_data10
    {P8, 37} ->  %% UART5_TXD  - U1  - MODE[0|1|2|3|4|6|7] 	- lcd_data8
    {P8, 38} ->  %% UART5_RXD  - U2  - MODE[0|1|2|3|4|6|7] 	- lcd_data9
    {P8, 39} ->  %% GPIO2_12   - T3  - MODE[0|1|3|7] 		- lcd_data6
    {P8, 40} ->  %% GPIO2_13   - T4  - MODE[0|1|3|4|7] 		- lcd_data7
    {P8, 41} ->  %% GPIO2_10   - T1  - MODE[0|1|3|7] 		- lcd_data4
    {P8, 42} ->  %% GPIO2_11   - T2  - MODE[0|1|3|7] 		- lcd_data5
    {P8, 43} ->  %% GPIO2_8    - R3  - MODE[0|1|3|7] 		- lcd_data2
    {P8, 44} ->  %% GPIO2_9    - R4  - MODE[0|1|3|7] 		- lcd_data3
    {P8, 45} ->  %% GPIO2_6    - R1  - MODE[0|1|3|7] 		- lcd_data0
    {P8, 46} ->  %% GPIO2_7    - R2  - MODE[0|1|3|7] 		- lcd_data1
    {P9, 1}  ->  %% GND
      {error, {"Reserved PIN", "This PIN is used for GND"}};
    {P9, 2}  ->  %% GND
      {error, {"Reserved PIN", "This PIN is used for GND"}};
    {P9, 3}  ->  %% DC_3.3V
      {error, {"Reserved PIN", "This PIN is used for DC_3.3V"}};
    {P9, 4}  ->  %% DC_3.3V
      {error, {"Reserved PIN", "This PIN is used for DC_3.3V"}};
    {P9, 5}  ->  %% VDD_5V
      {error, {"Reserved PIN", "This PIN is used for VDD_5V"}};
    {P9, 6}  ->  %% VDD_5V
      {error, {"Reserved PIN", "This PIN is used for VDD_5V"}};
    {P9, 7}  ->  %% SYS_5V
      {error, {"Reserved PIN", "This PIN is used for SYS_5V"}};
    {P9, 8}  ->  %% SYS_5V
      {error, {"Reserved PIN", "This PIN is used for SYS_5V"}};
    {P9, 9}  ->  %% PWR_BUT
      {error, {"Reserved PIN", "This PIN is used for PWR_BUT"}};
    {P9, 10} ->  %% SYS_RESETn - A10 - MODE[0]			- REST_OUT
      {error, {"Reserved PIN", "This PIN is used for SYS_RESETn"}};
    {P9, 11} ->  %% UART4_RXD  - T17 - MODE[0|1|2|3|4|6|7]	- gpmc_wait0
    {P9, 12} ->  %% GPIO1_28   - U18 - MODE[0|1|2|3|4|6|7]	- gpmc_ben1	
    {P9, 13} ->  %% UART4_TXD  - U17 - MODE[0|1|2|3|4|6|7]	- gpmc_wpn
    {P9, 14} ->  %% EHRPWM1A   - U14 - MODE[0|1|2|3|4|6|7]	- gpmc_a2
    {P9, 15} ->  %% GPIO1_16   - R13 - MODE[0|1|2|3|4|6|7]	- gpmc_a0
    {P9, 16} ->  %% EHRPWM1B   - T14 - MODE[0|1|2|3|4|6|7]	- gpmc_a3
    {P9, 17} ->  %% I2C1_SCL   - A16 - MODE[0|1|2|3|7]		- spi0_cs0
    {P9, 18} ->  %% I2C1_SDA   - B16 - MODE[0|1|2|3|7]		- spi0_d1
    {P9, 19} ->  %% I2C2_SCL   - D17 - MODE[0|1|2|3|4|7]	- uart1_rtsn
    {P9, 20} ->  %% I2C2_SDA   - D18 - MODE[0|1|2|3|4|7]	- uart1_ctsn
    {P9, 21} ->  %% UART2_TXD  - B17 - MODE[0|1|2|3|6|7]	- spi0_d0
    {P9, 22} ->  %% UART2_RXD  - A17 - MODE[0|1|2|3|6|7]	- spi0_sclk
    {P9, 23} ->  %% GPIO1_17   - V14 - MODE[0|1|2|3|4|6|7]	- gpmc_a1
    {P9, 24} ->  %% UART1_TXD  - D15 - MODE[0|1|2|3|7]		- uart1_txd
    {P9, 25} ->  %% GPIO3_21   - A14 - MODE[0|1|2|3|4|7]	- mcasp0_ahclkx
    {P9, 26} ->  %% UART1_RXD  - D16 - MODE[0|1|2|3|7]		- uart1_rxd
    {P9, 27} ->  %% GPIO3_19   - C13 - MODE[0|1|2|3|4|6|7]	- mcasp0_fsr
    {P9, 28} ->  %% SPI1_CS0   - C12 - MODE[0|1|2|3|4|7]	- mcasp0_ahclkr
    {P9, 29} ->  %% SPI1_D0    - B13 - MODE[0|1|3|4|7]		- mcasp0_fsx
    {P9, 30} ->  %% SPI1_D1    - D12 - MODE[0|1|3|4|7]		- mcasp0_axr0
    {P9, 31} ->  %% SPI1_SCLK  - A13 - MODE[0|1|3|4|7]		- mcasp0_aclkx
    {P9, 32} ->  %% VDD_ADC
      {error, {"Reserved PIN", "This PIN is used for VDD_ADC"}};
    {P9, 33} ->  %% AIN4       - C8  - MODE[]			- ain4
    {P9, 34} ->  %% GNDA_ADC
      {error, {"Reserved PIN", "This PIN is used for GNDA_ADC"}};
    {P9, 35} ->  %% AIN6       - A5  - MODE[]			- ain6
    {P9, 36} ->  %% AIN5       - A6  - MODE[]			- ain5
    {P9, 37} ->  %% AIN2       - B7  - MODE[]			- ain2
    {P9, 38} ->  %% AIN3       - A7  - MODE[]			- ain3
    {P9, 39} ->  %% AIN0       - B6  - MODE[]			- ain0
    {P9, 40} ->  %% AIN1       - C7  - MODE[]			- ain1
    {P9, 41} ->  %% CLKOUT2    - D14 - MODE[0|2|3|4|6|7]	- xdma_event_intr1
    {P9, 42} ->  %% GPIO0_7    - C18 - MODE[0|1|2|3|4|6|7]	- ecap0_in_pwm0_out
    {P9, 43} ->  %% GND
      {error, {"Reserved PIN", "This PIN is used for GND"}};
    {P9, 44} ->  %% GND
      {error, {"Reserved PIN", "This PIN is used for GND"}};
    {P9, 45} ->  %% GND
      {error, {"Reserved PIN", "This PIN is used for GND"}};
    {P9, 46} ->  %% GND
      {error, {"Reserved PIN", "This PIN is used for GND"}}
    end,

  {ok, FdExport} = file:open("/sys/class/gpio/export", [write]),
  file:write(FdExport, integer_to_list(Pin)),
  file:close(FdExport),

  {ok, FdPinDir} = file:open("/sys/class/gpio/gpio" ++ integer_to_list(Pin) ++ "/direction", [write]),
  case Direction of
    in  -> file:write(FdPinDir, "in");
    out -> file:write(FdPinDir, "out")
  end,
  file:close(FdPinDir),

  {ok, FdPinVal} = file:open("/sys/class/gpio/gpio" ++ integer_to_list(Pin) ++ "/value", [write]),
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
