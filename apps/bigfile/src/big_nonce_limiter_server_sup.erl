-module(big_nonce_limiter_server_sup).

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
	case big_config:is_vdf_server() of
		false ->
			ignore;
		true ->
			{ok, Config} = application:get_env(bigfile, config),
			Workers = lists:map(
				fun(Peer) ->
					Name = list_to_atom("big_nonce_limiter_server_worker_"
							++ big_util:peer_to_str(Peer)),
					?CHILD_WITH_ARGS(big_nonce_limiter_server_worker,
							worker, Name, [Name, Peer])
				end,
				Config#config.nonce_limiter_client_peers
			),
			Workers2 = [?CHILD(big_nonce_limiter_server, worker) | Workers],
			{ok, {{one_for_one, 5, 10}, Workers2}}
	end.
