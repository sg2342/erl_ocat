-module(erl_ocat_peer_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/0]).

-export([init/1]).

start_child() -> supervisor:start_child(?MODULE, []).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Child = {erl_ocat_peer, {erl_ocat_peer, start_link, []},
	   temporary, brutal_kill, worker, [erl_ocat_peer]},
    {ok, {{simple_one_for_one, 10, 1}, [Child]}}.
