-module(pin).
-export([read_gpio/1, readgpio/1, bin_to_hex/1, hex_to_bin/1]).

read_gpio(File) ->
  case file:read_file(File) of
    {ok, Res} ->
      io:format(" ~n File Name: ~p~n Results: ~p~n",[File,Res]);
    {error, Reason} ->
      io:format(" Error read_file() : ~p~n",[Reason])
  end.

readgpio(File) ->
  case file:open(File, [read, binary, raw]) of
    {ok, Fd} ->
      case file:pread(Fd, 47, 4) of
        eof ->
          io:format(" Opended the file but reached EOF unexpetedly.~n");
        {error, Reason} ->
          io:format(" Error file:pread() : ~p~n",[Reason]);
	{ok, Res} -> 
          io:format(" Results of file: ~p~n",[Res])
      end,
      file:close(Fd);
    {error, Reason} ->
      io:format(" Error file:open() : ~p~n",[Reason])
  end.


hex(N) when N < 10 -> $0+N;
hex(N) when N >= 10, N < 16 -> $a+(N-10).
to_hex(N) when N < 16 -> [hex(N rem 16)];
to_hex(N) when N >= 16, N < 256 -> [hex(N div 16), hex(N rem 16)].



hex_to_bin(L) ->
  binary:encode_unsigned(erlang:list_to_integer(L,16)).

bin_to_hex(L) ->
  B = binary:encode_unsigned(erlang:list_to_integer(L,2)),
  string:join([to_hex(X) || X <- erlang:bitstring_to_list(B)],"").
