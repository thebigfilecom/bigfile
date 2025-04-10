-module(big_tx_emitter_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-include_lib("bigfile/include/big_sup.hrl").
-include_lib("bigfile/include/big_config.hrl").

%%%===================================================================
%%% Public interface.
%%%===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks.
%% ===================================================================

init([]) ->
	{ok, Config} = application:get_env(bigfile, config),
	MaxEmitters = Config#config.max_emitters,
	Workers = lists:map(fun tx_workers/1, lists:seq(1, MaxEmitters)),
	WorkerNames = [ Name || #{ id := Name } <- Workers],
	Emitter = tx_emitter([big_tx_emitter, WorkerNames]),
	ChildrenSpec = [Emitter|Workers],
	{ok, {supervisor_spec(), ChildrenSpec}}.

supervisor_spec() ->
	#{ strategy => one_for_one
	 , intensity => 5
	 , period => 10
	 }.

% helper to create big_tx_emitter process, in charge
% of sending chunk to propagate to big_tx_emitter_worker.
tx_emitter(Args) ->
	#{ id => big_tx_emitter
	 , type => worker
	 , start => {big_tx_emitter, start_link, Args}
	 , shutdown => ?SHUTDOWN_TIMEOUT
	 , modules => [big_tx_emitter]
	 , restart => permanent
	 }.

% helper function to create ar_tx_workers processes.
tx_workers(Num) ->
	Name = "big_tx_emitter_worker_" ++ integer_to_list(Num),
	Atom = list_to_atom(Name),
	#{ id => Atom
	 , start => {big_tx_emitter_worker, start_link, [Atom]}
	 , restart => permanent
	 , type => worker
	 , timeout => ?SHUTDOWN_TIMEOUT
	 , modules => [big_tx_emitter_worker]
	 }.
