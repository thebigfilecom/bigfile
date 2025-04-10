-module(big_metrics_collector).

-behaviour(prometheus_collector).

-export([
	deregister_cleanup/1,
	collect_mf/2
]).

-import(prometheus_model_helpers, [create_mf/4]).

-include_lib("prometheus/include/prometheus.hrl").
-define(METRIC_NAME_PREFIX, "bigfile_").

%% ===================================================================
%% API
%% ===================================================================

%% called to collect Metric Families
-spec collect_mf(_Registry, Callback) -> ok when
	_Registry :: prometheus_registry:registry(),
	Callback :: prometheus_collector:callback().
collect_mf(_Registry, Callback) ->
	Metrics = metrics(),
	[add_metric_family(Metric, Callback) || Metric <- Metrics],
	ok.

%% called when collector deregistered
deregister_cleanup(_Registry) -> ok.

%% ===================================================================
%% Private functions
%% ===================================================================

add_metric_family({Name, Type, Help, Metrics}, Callback) ->
	Callback(create_mf(?METRIC_NAME(Name), Help, Type, Metrics)).

metrics() ->
	[
	 {storage_blocks_stored, gauge,
		"Blocks stored",
		case ets:lookup(big_header_sync, synced_blocks) of [] -> 0; [{_, N}] -> N end},
	 {bignode_queue_len, gauge,
		"Size of message queuee on big_node_worker",
		element(2, erlang:process_info(whereis(big_node_worker), message_queue_len))},
	 {bigbridge_queue_len, gauge,
		"Size of message queuee on big_bridge",
		element(2, erlang:process_info(whereis(big_bridge), message_queue_len))},
	 {ignored_ids_len, gauge,
		"Size of table of Ignored/already seen IDs:",
		ets:info(ignored_ids, size)},
	 {big_data_discovery_bytes_total, gauge, "big_data_discovery process memory",
		get_process_memory(big_data_discovery)},
	 {big_node_worker_bytes_total, gauge, "big_node_worker process memory",
		get_process_memory(big_node_worker)},
	 {big_header_sync_bytes_total, gauge, "big_header_sync process memory",
		get_process_memory(big_header_sync)},
	 {big_wallets_bytes_total, gauge, "big_wallets process memory",
		get_process_memory(big_wallets)}
	].

get_process_memory(Name) ->
	case whereis(Name) of
		undefined ->
			0;
		PID ->
			{memory, Memory} = erlang:process_info(PID, memory),
			Memory
	end.
