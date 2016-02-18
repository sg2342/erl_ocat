-module(erl_ocat_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Reg = {erl_ocat_reg, {erl_ocat_reg, start_link, []},
	   permanent, 2000, worker, [erl_ocat_reg]},
    Tun = {erl_ocat_tun, {erl_ocat_tun, start_link, []},
	   permanent, 2000, worker, [erl_ocat_tun]},
    Listener = {erl_ocat_listener, {erl_ocat_listener, start_link, []},
		permanent, 2000, worker, [erl_ocat_listener]},
    Acceptors = {erl_ocat_acceptor_sup, {erl_ocat_acceptor_sup, start_link, []},
		 permanent, infinity, supervisor, [erl_ocat_acceptor_sup]},
    Peers = {erl_ocat_peer_sup, {erl_ocat_peer_sup, start_link, []},
	     permanent, 2000, worker, [erl_ocat_peer_sup]},
    {ok, {{one_for_one, 1000, 3600}, [Reg, Tun, Listener, Acceptors, Peers]}}.
