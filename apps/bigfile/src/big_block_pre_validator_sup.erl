-module(big_block_pre_validator_sup).

-behaviour(supervisor).

-include_lib("bigfile/include/big_sup.hrl").
-include_lib("bigfile/include/big_config.hrl").

-export([start_link/0]).
-export([init/1]).

%%%===================================================================
%%% Public API.
%%%===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks.
%%%===================================================================

init([]) ->
	Children = [?CHILD(big_block_pre_validator, worker)],
	{ok, {{one_for_one, 5, 10}, Children}}.
