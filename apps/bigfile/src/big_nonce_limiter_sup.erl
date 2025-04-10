-module(big_nonce_limiter_sup).

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
	ServerWorkers = lists:map(
		fun(Peer) ->
			Name = list_to_atom("big_nonce_limiter_server_worker_"
					++ ar_util:peer_to_str(Peer)),
			?CHILD_WITH_ARGS(big_nonce_limiter_server_worker,
					worker, Name, [Name, Peer])
		end,
		Config#config.nonce_limiter_client_peers
	),
	Client = ?CHILD(big_nonce_limiter_client, worker),
	Server = ?CHILD(big_nonce_limiter_server, worker),
	NonceLimiter = ?CHILD(big_nonce_limiter, worker),

	Workers = case big_config:is_vdf_server() of
		true ->
			[NonceLimiter, Server, Client | ServerWorkers];
		false ->
			[NonceLimiter, Client]
	end,
	{ok, {{one_for_one, 5, 10}, Workers}}.
