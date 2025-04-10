%%
%% This file is only intended to be included into user_default.erl file.
%% The reason to incluide these headers into user_default module is to
%% enable records to be rendered properly in the REPL.
%% It might be a good idea to also include some third-party libraries headers
%% here as well (e.g. cowboy' request, etc.)
%%

-include_lib("bigfile/include/big.hrl").
-include_lib("bigfile/include/big_blacklist_middleware.hrl").
-include_lib("bigfile/include/big_block.hrl").
-include_lib("bigfile/include/big_chain_stats.hrl").
-include_lib("bigfile/include/big_chunk_storage.hrl").
-include_lib("bigfile/include/big_config.hrl").
-include_lib("bigfile/include/big_consensus.hrl").
-include_lib("bigfile/include/big_data_discovery.hrl").
-include_lib("bigfile/include/big_data_sync.hrl").
-include_lib("bigfile/include/big_header_sync.hrl").
-include_lib("bigfile/include/big_inflation.hrl").
-include_lib("bigfile/include/big_mining.hrl").
-include_lib("bigfile/include/big_p3.hrl").
-include_lib("bigfile/include/big_peers.hrl").
-include_lib("bigfile/include/big_poa.hrl").
-include_lib("bigfile/include/big_pool.hrl").
-include_lib("bigfile/include/big_pricing.hrl").
-include_lib("bigfile/include/big_sup.hrl").
-include_lib("bigfile/include/big_sync_buckets.hrl").
-include_lib("bigfile/include/big_vdf.hrl").
-include_lib("bigfile/include/big_verify_chunks.hrl").
-include_lib("bigfile/include/big_wallets.hrl").
