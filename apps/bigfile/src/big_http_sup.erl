%% This Source Code Form is subject to the terms of the GNU General
%% Public License, v. 2.0. If a copy of the GPLv2 was not distributed
%% with this file, You can obtain one at
%% https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html

-module(big_http_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("bigfile/include/big_sup.hrl").

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
	{ok, {{one_for_one, 5, 10}, [?CHILD(big_http, worker)]}}.
