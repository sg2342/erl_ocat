-module(erl_ocat_tun).

-behaviour(gen_server).

-export([start_link/0, fd/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(s, {port, fd}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

fd() -> gen_server:call(?MODULE, fd).

init([]) ->
    TunDev = case application:get_env(tundev) of
		 undefined -> "tun99";
		 {ok, [_|_] = T} -> T end,
    {ok, FD} = procket:dev(TunDev),
    TUNSIFHEAD = procket_ioctl:iow($t,96,4),
    {ok, _} =  procket:ioctl(FD, TUNSIFHEAD, <<1:32/native>>),
    Port = open_port({fd, FD, FD}, [stream, binary]),
    {ok, #s{fd = FD, port = Port}}.

handle_call(fd, _From, #s{fd = FD} = State) -> {reply, {ok, FD}, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Port, {data, <<0,0,0,28, Bin/binary>>}},
	    #s{port = Port} = State) ->
    _ = erl_ocat_peer:tun_in(Bin),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
