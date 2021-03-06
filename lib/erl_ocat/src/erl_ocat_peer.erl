-module(erl_ocat_peer).

-behaviour(gen_fsm).

-export([start_link/0, accepted/1, tun_in/1]).

-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([wait_accept_or_tun/2, wait_1st_from_socket/2, forward/2]).

-define(IPv6_HEADER_SIZE, 40).

%% FD60:DB4D:DDB5::/48
%%-define(Prefix, 16#FD60DB4DDDB5).

%% fd87:d87e:eb43::/48
-define(Prefix, 16#fd87d87eeb43).

-record(s, {socket, frag = <<>>, fd_pih, receiver_authentication}).

accepted(Socket) ->
    {ok, Pid} = erl_ocat_peer_sup:start_child(),
    ok = gen_tcp:controlling_process(Socket, Pid),
    gen_fsm:send_event(Pid, {accepted, Socket}).

tun_in(<< 16#6:4, _Class:8, _Flow:20, Len:16, _Next:8, _Hop:8,
	  ?Prefix:48, _Src:80, ?Prefix:48, Dst:80,
	  _Data:Len/binary >> = Pkt) ->
    case erl_ocat_reg:lookup(Dst) of
	undefined ->
	    {ok, Pid} = erl_ocat_peer_sup:start_child(),
	    gen_fsm:send_event(Pid, {tun, Dst, Pkt});
	Pid -> Pid ! {tun, Pkt}
    end;
tun_in(_) -> {error, invalid}.

to_onion(Dst) when is_integer(Dst) -> to_onion(<<Dst:80>>);
to_onion(<< _:6/binary, X:10/binary >>) -> to_onion(X);
to_onion(<< _:10/binary >> = Bin) ->
    Enc = fun(I) when is_integer(I), I >= 26, I =< 31 -> I + 24;
	     (I) when is_integer(I), I >= 0, I =< 25 -> I + $a
	  end,
    B0 = << <<(Enc(I))>> || <<I:5>> <= Bin >>,
    <<B0/binary, ".onion">>.

start_link() -> gen_fsm:start_link(?MODULE, [], []).

init([]) ->
    {ok, A} = application:get_env(receiver_authentication),
    {ok, wait_accept_or_tun, #s{receiver_authentication= A}, 1000}.

wait_accept_or_tun(timeout, State) ->
    {stop, normal, State};
wait_accept_or_tun({accepted, Socket},
		   #s{receiver_authentication = false} = State) ->
    next_state(wait_1st_from_socket, State#s{socket = Socket});
wait_accept_or_tun({accepted, Socket},
		   #s{receiver_authentication = true} = State) ->
    {ok, {FD, PIH}} = erl_ocat_tun:fd_pih(),
    next_state(forward, State#s{socket = Socket, fd_pih = {FD, PIH}});
wait_accept_or_tun({tun, Dst, Pkt}, #s{} = State) ->
    case erl_ocat_reg:insert(Dst) of
	false -> {stop, normal, State};
	true ->
	    {ok, {FD, PIH}} = erl_ocat_tun:fd_pih(),
	    case socks_connect(to_onion(Dst)) of
		{ok, Socket} ->
		    ok = gen_tcp:send(Socket, Pkt),
		    next_state(forward, State#s{socket = Socket,
						fd_pih = {FD, PIH}});
		{error, _} ->
		    {stop, normal, State}
	    end
    end.

wait_1st_from_socket(timeout, State) -> {stop, normal, State}.

forward(timeout, State) -> {stop, normal, State}.

-spec handle_event(_, _, _) -> no_return().
handle_event(Event, _StateName, _State) ->
    throw({unhandled_event, Event}).

-spec handle_sync_event(_, _, _, _) -> no_return().
handle_sync_event(Event, _From, _StateName, _State) ->
    throw({unhandled_sync_event, Event}).

handle_info({tun, Data}, forward, #s{socket = S} = State) ->
    case gen_tcp:send(S, Data) of
	{error, _} -> {stop, normal, State};
	ok -> {next_state, forward, State}
    end;
handle_info({tcp_error, S, _E}, _, #s{socket = S} = State) ->
    {stop, normal, State};
handle_info({tcp_closed, S}, _, #s{socket = S} = State) ->
    {stop, normal, State};
handle_info({tcp, S, Bin}, StateName, #s{frag = Frag, socket = S} = State)
  when (size(Bin) + size(Frag)) < ?IPv6_HEADER_SIZE ->
    next_state(StateName, State#s{frag = << Frag/binary, Bin/binary >>});
handle_info({tcp, S, Bin}, StateName, #s{socket = S, frag = Frag} = State) ->
    tcp_data(<< Frag/binary, Bin/binary >>, StateName, State#s{frag = <<>>});
handle_info(Info, _StateName, _State) ->
    throw({unhandled_info, Info}).

terminate(_Reason, _StateName, _State) -> ok.

code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

tcp_data(<< 16#6:4, _Class:8, _Flow:20, Len:16, _Next:8, _Hop:8,
	    ?Prefix:48, _Src:80, ?Prefix:48, _Dst:80, _Data:Len/binary,
	    Frag/binary >> = Bin,
	 forward, #s{} = State) ->
    tcp_data1(Len, Frag, Bin, State);
tcp_data(<< 6:4, _Class:8, _Flow:20, Len:16, _Next:8, _Hop:8, _Src:128,
	    _Dst:128, _Data:Len/binary, Frag/binary >>,
	 forward, State) -> % Prefix mismatch: drop
    next_state(forward, State#s{frag = Frag});
tcp_data(<< 6:4, _Class:8, _Flow:20, Len:16, _Next:8, _Hop:8,
	    ?Prefix:48, Src:80, ?Prefix:48, _Dst:80, _Data:Len/binary,
	    Frag/binary >> = Bin,
	 wait_1st_from_socket, #s{} = State) ->
    {ok, {FD, PIH}} = erl_ocat_tun:fd_pih(),
    case erl_ocat_reg:insert(Src) of
	false -> {stop, normal, State};
	true -> tcp_data1(Len, Frag, Bin, State#s{fd_pih = {FD, PIH}})
    end;
tcp_data(<< 6:4, _Class:8, _Flow:20, Len:16, _Next:8, _Hop:8, _Src:128,
	    _Dst:128, _Data:Len/binary, _Frag/binary >>,
	 wait_1st_from_socket, State) -> % Prefix mismatch: die
    {stop, normal, State};
tcp_data(Frag, StateName, #s{} = State) ->
    next_state(StateName, State#s{frag = Frag}).

tcp_data1(Len, Frag, Bin, #s{fd_pih = {FD, PIH}} = State) ->
    Sz = ?IPv6_HEADER_SIZE + Len,
    << Pkt:Sz/binary, _/binary >> = Bin,
    procket:write(FD, << PIH:4/binary, Pkt/binary >>),
    next_state(forward, State#s{frag = Frag}).

next_state(StateName, #s{socket = S} = State) ->
    ok = inet:setopts(S, [{active, once}]),
    {next_state, StateName, State}.

socks_connect(Address) ->
    {ok, TAddress} = application:get_env(socks_address),
    {ok, TPort} = application:get_env(socks_port),
    {ok, Port} = application:get_env(listen_port),
    Req = << 16#4, 16#1, Port:16, 16#1:32, 0, Address/binary, 0 >>,
    case gen_tcp:connect(TAddress, TPort, [binary, {active, false}]) of
	{error, _} = E -> E;
	{ok, Socket} ->
	    ok = gen_tcp:send(Socket, Req),
	    case gen_tcp:recv(Socket, 8, 4000) of
		{error, _} = E -> E;
		{ok, << 16#0, 16#5a, _ , _, _, _, _, _ >>} -> {ok, Socket};
		{ok, _} -> {error, socks_reply}
	    end
    end.
