{bigfileConfig, pkgs, ... }:

let
  inherit (pkgs) lib;
  filterTopLevelNulls = set:
    let
      isNotNull = value: value != null;
    in
    lib.filterAttrs (name: value: isNotNull value) set;
in
pkgs.writeText "config.json" (builtins.toJSON (filterTopLevelNulls {
  data_dir = bigfileConfig.dataDir;
  log_dir = bigfileConfig.logDir;
  storage_modules = bigfileConfig.storageModules;
  start_from_block_index = bigfileConfig.startFromBlockIndex;
  transaction_blacklists = bigfileConfig.transactionBlacklists;
  transaction_whitelists = bigfileConfig.transactionWhitelists;
  transaction_blacklist_urls = bigfileConfig.transactionBlacklistURLs;
  max_disk_pool_buffer_mb = bigfileConfig.maxDiskPoolBufferMb;
  max_disk_pool_data_root_buffer_mb = bigfileConfig.maxDiskPoolDataRootBufferMb;
  max_nonce_limiter_validation_thread_count = bigfileConfig.maxVDFValidationThreadCount;
  block_pollers = bigfileConfig.blockPollers;
  polling = bigfileConfig.polling;
  tx_validators = bigfileConfig.txValidators;
  disable = bigfileConfig.featuresDisable;
  enable = bigfileConfig.featuresEnable;
  header_sync_jobs = bigfileConfig.headerSyncJobs;
  sync_jobs = bigfileConfig.syncJobs;
  disk_pool_jobs = bigfileConfig.diskPoolJobs;
  debug = bigfileConfig.debug;
  packing_rate = bigfileConfig.packingRate;
  block_throttle_by_ip_interval = bigfileConfig.blockThrottleByIPInterval;
  block_throttle_by_solution_interval = bigfileConfig.blockThrottleBySolutionInterval;
  semaphores = {
    get_chunk = bigfileConfig.maxParallelGetChunkRequests;
    get_and_pack_chunk = bigfileConfig.maxParallelGetAndPackChunkRequests;
    get_tx_data = bigfileConfig.maxParallelGetTxDataRequests;
    post_chunk = bigfileConfig.maxParallelPostChunkRequests;
    get_block_index = bigfileConfig.maxParallelBlockIndexRequests;
    get_wallet_list = bigfileConfig.maxParallelWalletListRequests;
    get_sync_record = bigfileConfig.maxParallelGetSyncRecord;
    arql = 10;
    gateway_arql = 10;
  };
  requests_per_minute_limit = bigfileConfig.requestsPerMinuteLimit;
  max_connections = bigfileConfig.maxConnections;

  requests_per_minute_limit_by_ip = lib.lists.foldr
    (ipObj: acc: acc // {
      "${ipObj.ip}" = {
        chunk = ipObj.chunkLimit;
        data_sync_record = ipObj.dataSyncRecordLimit;
        default = ipObj.defaultLimit;
      };
    })
    { }
    bigfileConfig.requestsPerMinuteLimitByIp;
}))
