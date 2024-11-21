-module(big_data_sync_sup).

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
	SyncWorkers = case big_data_sync_worker_master:is_syncing_enabled() of
		true ->
			Workers = lists:map(
				fun(Number) ->
					Name = list_to_atom("big_data_sync_worker_" ++ integer_to_list(Number)),
					?CHILD_WITH_ARGS(big_data_sync_worker, worker, Name, [Name])
				end,
				lists:seq(1, Config#config.sync_jobs)
			),
			SyncWorkerNames = [element(1, El) || El <- Workers],
			SyncWorkerMaster = ?CHILD_WITH_ARGS(
				big_data_sync_worker_master, worker, big_data_sync_worker_master,
				[SyncWorkerNames]),
			Workers ++ [SyncWorkerMaster];
		false ->
			[]
	end,
	StorageModuleWorkers = lists:map(
		fun(StorageModule) ->
			StoreID = big_storage_module:id(StorageModule),
			StoreLabel = big_storage_module:label(StorageModule),
			Name = list_to_atom("big_data_sync_" ++ StoreLabel),
			?CHILD_WITH_ARGS(big_data_sync, worker, Name, [Name, {StoreID, none}])
		end,
		Config#config.storage_modules
	),
	DefaultStorageModuleWorker = ?CHILD_WITH_ARGS(big_data_sync, worker,
		big_data_sync_default, [big_data_sync_default, {"default", none}]),
	RepackInPlaceWorkers = lists:map(
		fun({StorageModule, TargetPacking}) ->
			StoreID = big_storage_module:id(StorageModule),
			Name = big_data_sync:name(StoreID),
			?CHILD_WITH_ARGS(big_data_sync, worker, Name, [Name, {StoreID, TargetPacking}])
		end,
		Config#config.repack_in_place_storage_modules
	),
	Children = SyncWorkers ++ StorageModuleWorkers ++ [DefaultStorageModuleWorker]
			++ RepackInPlaceWorkers,
	{ok, {{one_for_one, 5, 10}, Children}}.
