-module(erl_ocat_tun).

-behaviour(gen_server).

-export([start_link/0, fd_pih/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(s, {port, fd_pih}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

fd_pih() -> gen_server:call(?MODULE, fd_pih).

init([]) ->
    {ok, TunDev} = application:get_env(tundev),
    {FD, PIH} =
	case os:type() of
	    {unix, freebsd} ->
		{ok, FFD} = procket:dev(TunDev),
		TUNSIFHEAD = procket_ioctl:iow($t, 96, 4),
		{ok, _} =  procket:ioctl(FFD, TUNSIFHEAD, << 1:32/native >>),
		{FFD, << 0, 0, 0, 16#1C >>};
	    {unix, linux}->
		{ok, FFD} = procket:dev("net/tun"),
		Ifname = list_to_binary(TunDev),
		Flag = 16#0001,
		TUNSETIFF = procket_ioctl:iow($T, 202, 4),
		Bin = << Ifname/binary, 0:((15*8) - (byte_size(Ifname)*8)),
			 0:8, Flag:2/native-signed-integer-unit:8, 0:(14*8) >>,
		{ok, _} = procket:ioctl(FFD, TUNSETIFF, Bin),
		{FFD, << 0, 0,16#86dd:16 >>}
	end,
    Port = open_port({fd, FD, FD}, [stream, binary]),
    {ok, #s{port = Port, fd_pih = {FD, PIH}}}.

handle_call(fd_pih, _From, #s{fd_pih = {FD, PIH}} = State) ->
    {reply, {ok, {FD, PIH}}, State};
handle_call(Call, _From, _State) -> throw({unhandled_call, Call}).

-spec handle_cast(_, _) -> no_return().
handle_cast(Cast, _State) -> throw({unhandled_cast, Cast}).


handle_info({Port, {data, << PIH:4/binary, Bin/binary >>}},
	    #s{port = Port, fd_pih = {_, PIH}} = State) ->
    _ = erl_ocat_peer:tun_in(Bin),
    {noreply, State};
handle_info(Info, _State) -> throw({unhandled_info, Info}).

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
