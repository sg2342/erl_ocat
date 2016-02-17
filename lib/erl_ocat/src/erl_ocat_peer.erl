-module(erl_ocat_peer).

-behaviour(gen_fsm).

-export([start_link/0, start/1, tun_in/1]).


-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([wait_accept_or_tun/2, wait_accept_or_tun/3,
	 wait_1st_from_socket/2, wait_1st_from_socket/3,
	 forward/2, forward/3]).

-export([socks_connect/1]).

-record(s, {socket, frag = <<>>, fd}).

start(Socket) ->
    {ok, Pid} = erl_ocat_peer_sup:start_child(),
    ok = gen_tcp:controlling_process(Socket, Pid),
    gen_fsm:send_event(Pid, {accepted, Socket}).

tun_in(<< 6:4,_Class:8, _Flow:20, Len:16, _Next:8, _Hop:8,
	  _Source:128, Destination:16/binary, _Data:Len/binary >> = Pkt) ->
    N = to_onion(Destination),
    try N ! {tun, Pkt}, ok
    catch error:badarg ->
	    {ok, Pid} = erl_ocat_peer_sup:start_child(),
	    gen_fsm:send_event(Pid, {tun, N, Pkt})
    end;
tun_in(_) -> {error, invalid}.

to_onion(<<_:6/binary, X:10/binary>>) -> to_onion(X);
to_onion(<<_:10/binary>> = Bin) ->
    Enc = fun(I) when is_integer(I), I >= 26, I =< 31 -> I + 24;
	     (I) when is_integer(I), I >= 0, I =< 25 -> I + $a
	  end,
    B0 = << <<(Enc(I))>> || <<I:5>> <= Bin >>,
    list_to_atom(binary_to_list(B0) ++ ".onion").

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

init([]) ->
    {ok, wait_accept_or_tun, #s{}, 1000}.

wait_accept_or_tun(timeout, State) ->
    {stop, normal, State};
wait_accept_or_tun({accepted, Socket}, #s{} = State) ->
    next_state(wait_1st_from_socket, State#s{socket = Socket});
wait_accept_or_tun({tun, N, Pkt}, #s{} = State) ->
    register(N, self()),
    {ok, FD} = erl_ocat_tun:fd(),
    case socks_connect(N) of
	{ok, Socket} ->
	    ok = gen_tcp:send(Socket, Pkt),
	    next_state(forward, State#s{socket = Socket, fd = FD});
	{error, _} ->
	    {stop, normal, State}
    end.

wait_1st_from_socket(timeout, State) -> {stop, normal, State}.

forward(timeout, State) -> {stop, normal, State}.

wait_accept_or_tun(_Event, _From, State) ->
    {reply, ok, wait_accept_or_tun, State}.

wait_1st_from_socket(_Event, _From, State) ->
    {reply, ok, wait_1st_from_socket, State}.

forward(_Event, _From, State) ->
    {reply, ok, forward, State}.


handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

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
  when (size(Bin) + size(Frag)) < 40 ->
    next_state(StateName, State#s{frag = <<Frag/binary, Bin/binary>>});
handle_info({tcp, S, Bin}, StateName, #s{socket = S, frag = Frag} = State) ->
    tcp_data(<<Frag/binary, Bin/binary>>, StateName, State#s{frag = <<>>});
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

tcp_data(<<6:4, _Class:8, _Flow:20, Len:16, _Next:8, _Hop:8,
	   _Source:128, _Destination:128,
	   _Payload:Len/binary, Frag/binary>> = Bin,
	 forward, State) ->
    tcp_data1(Len, Frag, Bin, State);
tcp_data(<<6:4, _Class:8, _Flow:20, Len:16, _Next:8, _Hop:8,
	   Source:128/binary,
	   _Destination:128,
	   _Payload:Len/binary, Frag/binary>> = Bin,
	 wait_1st_from_socket, #s{} = State) ->
    {ok, FD} = erl_ocat_tun:fd(),
    register(to_onion(Source), self()),
    tcp_data1(Len, Frag, Bin, State#s{fd = FD});
tcp_data(Frag, StateName, #s{} = State) ->
    next_state(StateName, State#s{frag = Frag}).

tcp_data1(Len, Frag, Bin, #s{fd = FD} = State) ->
    Sz = 40 + Len,
    <<Pkt:Sz/binary, _/binary>> = Bin,
    procket:write(FD, <<0, 0, 0, 28, Pkt/binary>>),
    next_state(forward, State#s{frag = Frag}).

next_state(StateName, #s{socket = S} = State) ->
    ok = inet:setopts(S, [{active, once}]),
    {next_state, StateName, State}.

socks_connect(N) ->
    {ok, TAddress} = application:get_env(socks_address),
    {ok, TPort} = application:get_env(socks_port),
    {ok, Port} = application:get_env(listen_port),
    Address = list_to_binary(atom_to_list(N)),
    Req = <<4, 1, Port:16, 1:32, 0, Address/binary, 0>>,
    case gen_tcp:connect(TAddress, TPort, [binary, {active, false}]) of
	{error, _} = E -> E;
	{ok, Socket} ->
	    ok = gen_tcp:send(Socket, Req),
	    case gen_tcp:recv(Socket, 8, 4000) of
		{error, _} = E -> E;
		{ok, <<0,16#5a, _ , _, _, _, _, _>>} -> {ok, Socket};
		{ok, _} -> {error, socks_reply}
	    end
    end.
