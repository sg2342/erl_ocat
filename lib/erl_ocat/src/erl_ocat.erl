-module(erl_ocat).

-export([start/0, onion_to_v6/2, v6_to_onion/1, v6_to_addrstr/1]).

start() ->
    application:start(erl_ocat).

onion_to_v6(OnionString, {P, Q, R})
  when is_list(OnionString), length(OnionString) == 22 ->
    case lists:split(16, OnionString) of
	{HN, ".onion"} ->
	    <<A:16, B:16, C:16, D:16, E:16>> = dec(list_to_binary(HN), <<>>),
	    {ok, {P,Q,R,A,B,C,D,E}};
	_ -> {error, invalid}
    end;
onion_to_v6(_, _) -> {error, invalid}.

v6_to_addrstr({_,_,_,_,_,_,_,_} = Addr) ->
    Fmt = string:join(lists:duplicate(8,"~4.16.0b"),":"),
    lists:flatten(io_lib:format(Fmt, tuple_to_list(Addr))).

v6_to_onion({_,_,_,A,B,C,D,E}) ->
    binary_to_list(enc(<<A:16,B:16,C:16,D:16,E:16>>)) ++ ".onion".

enc(Bin) ->
    Enc = fun(I) when is_integer(I), I >= 26, I =< 31 -> I + 24;
	     (I) when is_integer(I), I >= 0, I =< 25 -> I + $a
	  end,
    << <<(Enc(I))>> || <<I:5>> <= Bin >>.

dec(<<>>, Bin) -> Bin;
dec(<<X, Rest/binary>>, Bits) ->
    Dec = fun(I) when I >= $2, I =< $7 -> I - 24;
	     (I) when I >= $a, I =< $z -> I - $a;
	     (I) when I >= $A, I =< $Z -> I - $A end,
    dec(Rest, <<Bits/bits, (Dec(X)):5>>).

