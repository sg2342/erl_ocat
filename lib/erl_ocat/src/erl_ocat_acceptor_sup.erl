-module(erl_ocat_acceptor_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, Acceptors} = application:get_env(acceptors),
    Childs = [{V, {erl_ocat_acceptor, start_link, []}, permanent, 2000, worker,
	       [erl_ocat_acceptor]} || V <- lists:seq(1, Acceptors)],
    {ok, {{one_for_one, 1000, 3600}, Childs}}.
