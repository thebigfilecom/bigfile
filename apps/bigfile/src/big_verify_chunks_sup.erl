-module(big_verify_chunks_sup).

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
	case Config#config.verify of
		false ->
			ignore;
		true ->
			Workers = lists:map(
				fun(StorageModule) ->
					StoreID = big_storage_module:id(StorageModule),
					Name = big_verify_chunks:name(StoreID),
					?CHILD_WITH_ARGS(big_verify_chunks, worker, Name, [Name, StoreID])
				end,
				Config#config.storage_modules
			),
			Reporter = ?CHILD(big_verify_chunks_reporter, worker),
			{ok, {{one_for_one, 5, 10}, [Reporter | Workers]}}
	end.
	
