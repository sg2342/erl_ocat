-module(erl_ocat_reg).

-behaviour(gen_server).

-export([start_link/0]).

-export([insert/1, lookup/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(s, {}).

-define(TAB, ?MODULE).

-type key() :: term().

-spec lookup(key()) -> pid() | undefined.
lookup(Key) ->
    case ets:lookup(?TAB, {k, Key}) of
	[{{k, Key}, Pid}] -> Pid;
	[] -> undefined
    end.

-spec insert(key()) -> true | false | {error, _}.
insert(K) -> gen_server:call(?MODULE, {insert, K, self()}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ?TAB = ets:new(?TAB, [ordered_set, protected, named_table,
			  {read_concurrency, true}]),
    {ok, #s{}}.

handle_call({insert, Key, Pid}, _, State) ->

    R = ets:insert_new(?TAB, {{k, Key}, Pid}) andalso
	ets:insert(?TAB, {{r, erlang:monitor(process, Pid)}, Key}),
    {reply, R, State};
handle_call(Call, _From, _State) -> throw({unhandled_call, Call}).

-spec handle_cast(_, _) -> no_return().
handle_cast(Cast, _State) -> throw({unhandled_cast, Cast}).

handle_info({'DOWN', Ref, process, _, _}, State) ->
    case ets:lookup(?TAB, {r, Ref}) of
	[] -> true;
	[{{r, Ref}, Key}] ->
	    ets:delete(?TAB, {r, Ref}),
	    ets:delete(?TAB, {k, Key})
    end,
    {noreply, State};
handle_info(Info, _State) -> throw({unhandled_info, Info}).

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
