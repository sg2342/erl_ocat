-module(erl_ocat_acceptor).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(EMFILE_BACKOFF, 300). % wait 300 ms when receiving emfile on accept

-record(s, {}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, #s{}, 0}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, #s{} = State) ->
    Delay = fun({error, emfile}) -> ?EMFILE_BACKOFF;
	       ({error, closed}) -> ?EMFILE_BACKOFF;
	       (_) -> 0
	    end,
    D = case erl_ocat_listener:socket() of
	    {error, _} = E -> E;
	    {ok, ListenSocket} -> do_accept(ListenSocket)
	end,
    {noreply, State, Delay(D)}.

do_accept(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
	{error, _} = E -> E;
	{ok, Socket} -> erl_ocat_peer:start(Socket)
    end.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
