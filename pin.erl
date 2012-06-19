-module(pin).
-export([read_gpio/1, readgpio/1, bin_to_hex/1, hex_to_bin/1, save_current_state/1, ext_mode/1 ]).

readgpio(File) ->
  case file:read_file(File) of
    {ok, Res} ->
      io:format(" ~n File Name: ~p~n Results: ~p~n",[File,Res]);
    {error, Reason} ->
      io:format(" Error read_file() : ~p~n",[Reason])
  end.

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


ext_mode([]) -> string:join(["0","0","0"],"");
ext_mode([Pin_Slew]) ->
  if Pin_Slew == 0 ; Pin_Slew == 1 -> string:join([erlang:integer_to_list(Pin_Slew),"0","0"],"");    
    true -> {error,"Invalid Ext_Mode Value Pin_Slew"}
  end;
ext_mode([Pin_Slew,Pin_UpDown]) ->
  if Pin_Slew == 0 ; Pin_Slew == 1,
     Pin_UpDown == 0 ; Pin_UpDown == 1 -> string:join([erlang:integer_to_list(Pin_Slew),erlang:integer_to_list(Pin_UpDown),"0"],"");
    true -> {error,"Invalid Ext_Mode Value Pin_Slew or Pin_UpDown"}
  end;
ext_mode([Pin_Slew,Pin_UpDown,Pin_UpDown_E]) ->
  if Pin_Slew == 0 ; Pin_Slew == 1,
     Pin_UpDown == 0 ; Pin_UpDown == 1,
     Pin_UpDown_E == 0 ; Pin_UpDown_E == 1 -> string:join([erlang:integer_to_list(Pin_Slew),erlang:integer_to_list(Pin_UpDown),erlang:integer_to_list(Pin_UpDown_E)],"");  
    true -> {error,"Invalid Ext_Mode Value Pin_Slew or Pin_UpDown or Pin_UpDown_E"}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%      Can use re:split in place of Token         %%
%% re:split("Erlang","[ln]",[{return,list},trim]). %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


save_current_state(File) ->
  {ok,Raw_File} = read_gpio(File),
  [_Name,Reg,_Other] = string:tokens(binary:bin_to_list(Raw_File),"()"),
  [_Other_Reg,Cur_State] = string:tokens(Reg,erlang:atom_to_list(' = ')),    
  [_,Cur] = string:tokens(Cur_State,"x"),
  S = erlang:bit_size(erlang:list_to_binary(Cur)),
  <<Current_State:S/integer-little>>=erlang:list_to_binary(Cur),
  Current_State.



hex(N) when N < 10 -> $0+N;
hex(N) when N >= 10, N < 16 -> $a+(N-10).
to_hex(N) when N < 16 -> [hex(N rem 16)];
to_hex(N) when N >= 16, N < 256 -> [hex(N div 16), hex(N rem 16)].



hex_to_bin(L) ->
  binary:encode_unsigned(erlang:list_to_integer(L,16)).

bin_to_hex(L) ->
  B = binary:encode_unsigned(erlang:list_to_integer(L,2)),
  string:join([to_hex(X) || X <- erlang:bitstring_to_list(B)],"").
