%% This Source Code Form is subject to the terms of the GNU General
%% Public License, v. 2.0. If a copy of the GPLv2 was not distributed
%% with this file, You can obtain one at
%% https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html

-module(big_events_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("bigfile/include/big_sup.hrl").

%% Helper macro for declaring children of supervisor.
-define(CHILD(Mod, I, Type), {I, {Mod, start_link, [I]}, permanent, ?SHUTDOWN_TIMEOUT, Type,
		[Mod]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	{ok, Config} = application:get_env(bigfile, config),
	{ok, {{one_for_one, 5, 10}, [
		%% Events: remaining_disk_space.
		?CHILD(big_events, disksup, worker),
		%% Events: new, ready_for_mining, orphaned, emitting_scheduled,
		%% preparing_unblacklisting, ready_for_unblacklisting, registered_offset.
		?CHILD(big_events, tx, worker),
		%% Events: discovered, rejected, new, double_signing, mined_block_received.
		?CHILD(big_events, block, worker),
		%% Events: unpacked, packed.
		?CHILD(big_events, chunk, worker),
		%% Events: removed
		?CHILD(big_events, peer, worker),
		%% Events: account_tree_initialized, initialized,
		%% new_tip, checkpoint_block, search_space_upper_bound.
		?CHILD(big_events, node_state, worker),
		%% Events: initialized, valid, invalid, validation_error, refuse_validation,
		%% computed_output.
		?CHILD(big_events, nonce_limiter, worker),
		%% Events: removed_file.
		?CHILD(big_events, chunk_storage, worker),
		%% Events: add_range, remove_range, global_remove_range, cut, global_cut.
		?CHILD(big_events, sync_record, worker),
		%% Events: rejected, stale, partial, accepted.
		?CHILD(big_events, solution, worker),
		%% Used for the testing purposes.
		?CHILD(big_events, testing, worker)
	]}}.
