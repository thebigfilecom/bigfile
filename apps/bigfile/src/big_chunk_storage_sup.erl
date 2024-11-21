-module(big_chunk_storage_sup).

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
	ets:new(chunk_storage_file_index, [set, public, named_table, {read_concurrency, true}]),
	{ok, Config} = application:get_env(bigfile, config),
	ConfiguredWorkers = lists:map(
		fun(StorageModule) ->
			StoreID = big_storage_module:id(StorageModule),
			Label = big_storage_module:label(StorageModule),
			Name = list_to_atom("big_chunk_storage_" ++ Label),
			?CHILD_WITH_ARGS(big_chunk_storage, worker, Name, [Name, {StoreID, none}])
		end,
		Config#config.storage_modules
	),
	DefaultChunkStorageWorker = ?CHILD_WITH_ARGS(big_chunk_storage, worker,
		big_chunk_storage_default, [big_chunk_storage_default, {"default", none}]),
	RepackInPlaceWorkers = lists:map(
		fun({StorageModule, Packing}) ->
			StoreID = big_storage_module:id(StorageModule),
            %% Note: the config validation will prevent a StoreID from being used in both
            %% `storage_modules` and `repack_in_place_storage_modules`, so there's
            %% no risk of a `Name` clash with the workers spawned above.
			Label = big_storage_module:label(StorageModule),
			Name = list_to_atom("big_chunk_storage_" ++ Label),
			?CHILD_WITH_ARGS(big_chunk_storage, worker, Name, [Name, {StoreID, Packing}])
		end,
		Config#config.repack_in_place_storage_modules
	),
	Workers = [DefaultChunkStorageWorker] ++ ConfiguredWorkers ++ RepackInPlaceWorkers,
	{ok, {{one_for_one, 5, 10}, Workers}}.
