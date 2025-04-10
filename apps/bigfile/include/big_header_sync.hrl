%% The frequency of processing items in the queue.
-ifdef(BIG_TEST).
-define(PROCESS_ITEM_INTERVAL_MS, 100).
-else.
-define(PROCESS_ITEM_INTERVAL_MS, 100).
-endif.

%% The frequency of checking if there are headers to sync after everything
%% is synced. Also applies to a fresh node without any data waiting for a block index.
%% Another case is when the process misses a few blocks (e.g. blocks were sent while the
%% supervisor was restarting it after a crash).
-ifdef(BIG_TEST).
-define(CHECK_AFTER_SYNCED_INTERVAL_MS, 5000).
-else.
-define(CHECK_AFTER_SYNCED_INTERVAL_MS, 5000).
-endif.

%% The initial value for the exponential backoff for failing requests.
-ifdef(BIG_TEST).
-define(INITIAL_BACKOFF_INTERVAL_S, 30).
-else.
-define(INITIAL_BACKOFF_INTERVAL_S, 30).
-endif.

%% The maximum exponential backoff interval for failing requests.
-ifdef(BIG_TEST).
-define(MAX_BACKOFF_INTERVAL_S, 2 * 60 * 60).
-else.
-define(MAX_BACKOFF_INTERVAL_S, 2 * 60 * 60).
-endif.

%% The frequency of storing the server state on disk.
-define(STORE_HEADER_STATE_FREQUENCY_MS, 30000).
