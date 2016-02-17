-module(erl_ocat_acceptor).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(EMFILE_BACKOFF, 300). % wait 300 ms when receiving emfile on accept

-record(s, {}).

start_link() -> gen_server:start_link(?MODULE, [], []).

init([]) -> {ok, #s{}, 0}.

-spec handle_call(_, _, _) -> no_return().
handle_call(Call, _From, _State) -> throw({unhandled_call, Call}).

-spec handle_cast(_, _) -> no_return().
handle_cast(Cast, _State) -> throw({unhandled_cast, Cast}).

handle_info(timeout, #s{} = State) ->
    Delay = fun({error, emfile}) -> ?EMFILE_BACKOFF;
	       ({error, closed}) -> ?EMFILE_BACKOFF;
	       (_) -> 0
	    end,
    D = case erl_ocat_listener:socket() of
	    {error, _} = E -> E;
	    {ok, ListenSocket} -> do_accept(ListenSocket)
	end,
    {noreply, State, Delay(D)};
handle_info(Info, _State) -> throw({unhandled_info, Info}).

do_accept(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
	{error, _} = E -> E;
	{ok, Socket} -> erl_ocat_peer:accepted(Socket)
    end.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
