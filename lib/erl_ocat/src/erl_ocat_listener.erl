-module(erl_ocat_listener).

-behaviour(gen_server).

-export([start_link/0, socket/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(s, {socket}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

socket() -> gen_server:call(?MODULE, socket).

init([]) ->
    {ok, Socket} = listen(),
    {ok, #s{socket = Socket}}.

handle_call(socket, _From, #s{socket = Socket} = State) ->
    {reply, {ok, Socket}, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

listen() ->
    LOpts = [binary, {packet, 0}, {active, false}, {reuseaddr, true},
	     {nodelay, true}, {backlog, 256}],
    {ok, Port} = application:get_env(listen_port),
    {ok, Address} = application:get_env(listen_address),
    gen_tcp:listen(Port, [{ip, Address}| LOpts]).
