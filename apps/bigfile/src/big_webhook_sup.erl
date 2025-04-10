%% This Source Code Form is subject to the terms of the GNU General
%% Public License, v. 2.0. If a copy of the GPLv2 was not distributed
%% with this file, You can obtain one at
%% https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html

-module(big_webhook_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("bigfile/include/big_sup.hrl").
-include_lib("bigfile/include/big_config.hrl").

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
	Children = lists:map(
		fun
			(Hook) when is_record(Hook, config_webhook) ->
				Handler = {big_webhook, Hook#config_webhook.url},
				{Handler, {big_webhook, start_link, [Hook]},
					permanent, ?SHUTDOWN_TIMEOUT, worker, [big_webhook]};
			(Hook) ->
				?LOG_ERROR([{event, failed_to_parse_webhook_config},
					{webhook_config, io_lib:format("~p", [Hook])}])
		end,
		Config#config.webhooks
	),
	{ok, {{one_for_one, 5, 10}, Children}}.
