-module(erl_ocat_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) -> erl_ocat_sup:start_link().

stop(_State) -> ok.
