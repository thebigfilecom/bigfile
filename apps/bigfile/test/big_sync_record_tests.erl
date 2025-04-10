-module(big_sync_record_tests).

-include_lib("bigfile/include/big.hrl").
-include_lib("bigfile/include/big_consensus.hrl").
-include_lib("eunit/include/eunit.hrl").

sync_record_test_() ->
	[
		{timeout, 120, fun test_sync_record/0},
		{timeout, 120, fun test_sync_record_with_replica_2_9/0}
	].

test_sync_record() ->
	SleepTime = 1000,
	DiskPoolStart = ?PARTITION_SIZE,
	PartitionStart = ?PARTITION_SIZE - ?DATA_CHUNK_SIZE,
	WeaveSize = 4 * ?DATA_CHUNK_SIZE,
	[B0] = ar_weave:init([], 1, WeaveSize),
	RewardAddr = big_wallet:to_address(big_wallet:new_keyfile()),
	{ok, Config} = application:get_env(bigfile, config),
	try
		Partition = {?PARTITION_SIZE, 0, {composite, RewardAddr, 1}},
		PartitionID = big_storage_module:id(Partition),
		StorageModules = [Partition],
		big_test_node:start(B0, RewardAddr, Config, StorageModules),
		Options = #{ format => etf, random_subset => false },

		%% Genesis data only
		{ok, Binary1} = big_global_sync_record:get_serialized_sync_record(Options),
		{ok, Global1} = big_intervals:safe_from_etf(Binary1),

		?assertEqual([{1048576, 0}], big_intervals:to_list(Global1)),
		?assertEqual(not_found,
			big_sync_record:get_interval(DiskPoolStart+1, big_data_sync, "default")),
		?assertEqual({1048576, 0}, big_sync_record:get_interval(1, big_data_sync, PartitionID)),

		%% Add a diskpool chunk
		big_sync_record:add(
			DiskPoolStart+?DATA_CHUNK_SIZE, DiskPoolStart, big_data_sync, "default"),
		timer:sleep(SleepTime),
		{ok, Binary2} = big_global_sync_record:get_serialized_sync_record(Options),
		{ok, Global2} = big_intervals:safe_from_etf(Binary2),

		?assertEqual([{1048576, 0},{DiskPoolStart+?DATA_CHUNK_SIZE,DiskPoolStart}],
			big_intervals:to_list(Global2)),
		?assertEqual({DiskPoolStart+?DATA_CHUNK_SIZE,DiskPoolStart},
			big_sync_record:get_interval(DiskPoolStart+1, big_data_sync, "default")),
		?assertEqual({1048576, 0}, big_sync_record:get_interval(1, big_data_sync, PartitionID)),

		%% Remove the diskpool chunk
			big_sync_record:delete(
			DiskPoolStart+?DATA_CHUNK_SIZE, DiskPoolStart, big_data_sync, "default"),
		timer:sleep(SleepTime),
		{ok, Binary3} = big_global_sync_record:get_serialized_sync_record(Options),
		{ok, Global3} = big_intervals:safe_from_etf(Binary3),
		?assertEqual([{1048576, 0},{DiskPoolStart+?DATA_CHUNK_SIZE,DiskPoolStart}],
			big_intervals:to_list(Global3)),
		%% We need to explicitly declare global removal
		big_events:send(sync_record,
				{global_remove_range, DiskPoolStart+?DATA_CHUNK_SIZE, DiskPoolStart}),
		true = ar_util:do_until(
				fun() ->
					{ok, Binary4} = big_global_sync_record:get_serialized_sync_record(Options),
					{ok, Global4} = big_intervals:safe_from_etf(Binary4),
					[{1048576, 0}] == big_intervals:to_list(Global4) end,
				200,
				5000),

		%% Add a storage module chunk
		big_sync_record:add(
			PartitionStart+?DATA_CHUNK_SIZE, PartitionStart, big_data_sync, PartitionID),
		timer:sleep(SleepTime),
		{ok, Binary5} = big_global_sync_record:get_serialized_sync_record(Options),
		{ok, Global5} = big_intervals:safe_from_etf(Binary5),

		?assertEqual([{1048576, 0},{PartitionStart+?DATA_CHUNK_SIZE,PartitionStart}],
			big_intervals:to_list(Global5)),
		?assertEqual(not_found,
			big_sync_record:get_interval(DiskPoolStart+1, big_data_sync, "default")),
		?assertEqual({1048576, 0}, big_sync_record:get_interval(1, big_data_sync, PartitionID)),
		?assertEqual({PartitionStart+?DATA_CHUNK_SIZE, PartitionStart},
				big_sync_record:get_interval(PartitionStart+1, big_data_sync, PartitionID)),

		%% Remove the storage module chunk
		big_sync_record:delete(
			PartitionStart+?DATA_CHUNK_SIZE, PartitionStart, big_data_sync, PartitionID),
		timer:sleep(SleepTime),
		?assertEqual([{1048576, 0},{PartitionStart+?DATA_CHUNK_SIZE,PartitionStart}],
			big_intervals:to_list(Global5)),
		big_events:send(sync_record,
				{global_remove_range, PartitionStart+?DATA_CHUNK_SIZE,PartitionStart}),
		true = ar_util:do_until(
				fun() ->
					{ok, Binary6} = big_global_sync_record:get_serialized_sync_record(Options),
					{ok, Global6} = big_intervals:safe_from_etf(Binary6),
					[{1048576, 0}] == big_intervals:to_list(Global6) end,
				200,
				1000),
		?assertEqual(not_found,
			big_sync_record:get_interval(DiskPoolStart+1, big_data_sync, "default")),
		?assertEqual({1048576, 0}, big_sync_record:get_interval(1, big_data_sync, PartitionID)),
		?assertEqual(not_found,
				big_sync_record:get_interval(PartitionStart+1, big_data_sync, PartitionID)),

		%% Add chunk to both diskpool and storage module
		big_sync_record:add(
			PartitionStart+?DATA_CHUNK_SIZE, PartitionStart, big_data_sync, "default"),
		big_sync_record:add(
			PartitionStart+?DATA_CHUNK_SIZE, PartitionStart, big_data_sync, PartitionID),
		timer:sleep(SleepTime),
		{ok, Binary6} = big_global_sync_record:get_serialized_sync_record(Options),
		{ok, Global6} = big_intervals:safe_from_etf(Binary6),

		?assertEqual([{1048576, 0}, {PartitionStart+?DATA_CHUNK_SIZE,PartitionStart}],
			big_intervals:to_list(Global6)),
		?assertEqual({PartitionStart+?DATA_CHUNK_SIZE,PartitionStart},
			big_sync_record:get_interval(PartitionStart+1, big_data_sync, "default")),
		?assertEqual({1048576, 0}, big_sync_record:get_interval(1, big_data_sync, PartitionID)),
		?assertEqual({PartitionStart+?DATA_CHUNK_SIZE, PartitionStart},
			big_sync_record:get_interval(PartitionStart+1, big_data_sync, PartitionID)),

		%% Now remove it from just the diskpool
		big_sync_record:delete(
			PartitionStart+?DATA_CHUNK_SIZE, PartitionStart, big_data_sync, "default"),
		timer:sleep(SleepTime),
		{ok, Binary7} = big_global_sync_record:get_serialized_sync_record(Options),
		{ok, Global7} = big_intervals:safe_from_etf(Binary7),

		?assertEqual([{1048576, 0}, {PartitionStart+?DATA_CHUNK_SIZE,PartitionStart}],
			big_intervals:to_list(Global7)),
		?assertEqual(not_found,
			big_sync_record:get_interval(DiskPoolStart+1, big_data_sync, "default")),
		?assertEqual({1048576, 0}, big_sync_record:get_interval(1, big_data_sync, PartitionID)),
		?assertEqual({PartitionStart+?DATA_CHUNK_SIZE, PartitionStart},
			big_sync_record:get_interval(PartitionStart+1, big_data_sync, PartitionID)),

		big_test_node:stop()
	after
		ok = application:set_env(bigfile, config, Config)
	end.


test_sync_record_with_replica_2_9() when ?BLOCK_2_9_SYNCING ->
	SleepTime = 1000,
	PartitionStart = ?PARTITION_SIZE - ?DATA_CHUNK_SIZE,
	WeaveSize = 4 * ?DATA_CHUNK_SIZE,
	[B0] = ar_weave:init([], 1, WeaveSize),
	RewardAddr = big_wallet:to_address(big_wallet:new_keyfile()),
	{ok, Config} = application:get_env(bigfile, config),
	try
		Partition = {?PARTITION_SIZE, 0, {replica_2_9, RewardAddr}},
		PartitionID = big_storage_module:id(Partition),
		StorageModules = [Partition],
		big_test_node:start(B0, RewardAddr, Config, StorageModules),
		Options = #{ format => etf, random_subset => false },

		%% Genesis data only
		{ok, Binary1} = big_global_sync_record:get_serialized_sync_record(Options),
		{ok, Global1} = big_intervals:safe_from_etf(Binary1),

		?assertEqual([], big_intervals:to_list(Global1)),
		?assertEqual({1048576, 0}, big_sync_record:get_interval(1, big_data_sync, PartitionID)),

		%% Add a storage module chunk
		big_sync_record:add(
			PartitionStart+?DATA_CHUNK_SIZE, PartitionStart, big_data_sync, PartitionID),
		timer:sleep(SleepTime),
		{ok, Binary5} = big_global_sync_record:get_serialized_sync_record(Options),
		{ok, Global5} = big_intervals:safe_from_etf(Binary5),

		?assertEqual([], big_intervals:to_list(Global5)),
		?assertEqual({1048576, 0}, big_sync_record:get_interval(1, big_data_sync, PartitionID)),
		?assertEqual({PartitionStart+?DATA_CHUNK_SIZE, PartitionStart},
				big_sync_record:get_interval(PartitionStart+1, big_data_sync, PartitionID)),

		big_test_node:stop()
	after
		ok = application:set_env(bigfile, config, Config)
	end;
test_sync_record_with_replica_2_9() -> ok.
