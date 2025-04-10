-module(big_pricing_tests).

-include_lib("bigfile/include/big.hrl").
-include_lib("bigfile/include/big_pricing.hrl").
-include_lib("bigfile/include/big_config.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DISTANT_FUTURE_BLOCK_HEIGHT, 262800000). %% 1,000 years from genesis

get_price_per_gib_minute_test_() ->
	[
		{timeout, 30, fun test_price_per_gib_minute_pre_block_time_history/0},
		big_test_node:test_with_mocked_functions(
			[
				{big_fork, height_2_7_2, fun() -> 10 end},
				{big_pricing_transition, transition_start_2_6_8, fun() -> 5 end},
				{big_pricing_transition, transition_start_2_7_2, fun() -> 15 end},
				{big_pricing_transition, transition_length_2_6_8, fun() -> 20 end},
				{big_pricing_transition, transition_length_2_7_2, fun() -> 40 end}
			],
			fun test_price_per_gib_minute_transition_phases/0),
		{timeout, 30, fun test_v2_price/0},
		big_test_node:test_with_mocked_functions(
			[
				{big_difficulty, poa1_diff_multiplier, fun(_) -> 2 end}
			],
			fun test_v2_price_with_poa1_diff_multiplier/0)
	].

%% @doc This test verifies an edge case code path that probably shouldn't ever be triggered.
%% big_fork:height_2_7() and big_fork:height_2_6_8() are 0
%% ?BLOCK_TIME_HISTORY_BLOCKS is 3
%% ?PRICE_2_6_8_TRANSITION_START is 2
%% So when the price transition starts we don't have enough block time history to apply the
%% new algorithm.
test_price_per_gib_minute_pre_block_time_history() ->
	Start = big_pricing_transition:transition_start_2_6_8(),
	B = #block{
		reward_history = reward_history(1, 1), block_time_history = block_time_history(1, 1) },
	?assertEqual(big_pricing_transition:static_price(),
		big_pricing:get_price_per_gib_minute(Start, B),
		"Before we have enough block time history").

test_price_per_gib_minute_transition_phases() ->
	%% V2 price when calculated with:
	%% - reward_history(1, 1)
	%% - block_time_history(1, 1)
	%% - PoA1 difficulty multiplier of 1
	B = #block{
		reward_history = reward_history(1, 1), block_time_history = block_time_history(1, 1) },
	V2Price = 61440,
	?assertEqual(V2Price,
		big_pricing:get_v2_price_per_gib_minute(10, B),
		"V2 Price"),
	%% Static price
	?assertEqual(big_pricing_transition:static_price(),
		big_pricing:get_price_per_gib_minute(4, B),
		"Static price"),
	%% 2.6.8 start
	?assertEqual(big_pricing_transition:static_price(),
		big_pricing:get_price_per_gib_minute(5, B),
		"2.6.8 start price"),
	%% 2.6.8 transition, pre-2.7.2
	%% 1/20 interpolation from 8162 to 61440
	?assertEqual(10825,
		big_pricing:get_price_per_gib_minute(6, B),
		"2.6.8 transition, pre-2.7.2 activation price"),
	%% 2.6.8 transition, post-2.7.2, pre-cap
	%% 5/20 interpolation from 8162 to 61440
	?assertEqual(21481,
		big_pricing:get_price_per_gib_minute(10, B),
		"2.6.8 transition, at 2.7.2 activation price"),
	%% 6/20 interpolation from 8162 to 61440
	?assertEqual(24145,
		big_pricing:get_price_per_gib_minute(11, B),
		"2.6.8 transition, post 2.7.2 activation, pre-cap price"),
	%% 2.6.8 transition, post-2.7.2, post-cap
	?assertEqual(30_000,
		big_pricing:get_price_per_gib_minute(14, B),
		"2.6.8 transition, post 2.7.2 activation, post-cap price"),
	%% 2.7.2 start
	?assertEqual(30_000,
		big_pricing:get_price_per_gib_minute(15, B),
		"2.7.2 start price"),
	%% 2.7.2 transition, before 2.6.8 end
	%% 5/40 interpolation from 30000 to 61440
	?assertEqual(33930,
		big_pricing:get_price_per_gib_minute(20, B),
		"2.7.2 transition price, before 2.6.8 end"),
	%% 2.7.2 transition, at 2.6.8 end
	%% 10/40 interpolation from 30000 to 61440
	?assertEqual(37860,
		big_pricing:get_price_per_gib_minute(25, B),
		"2.7.2 transition price, at 2.6.8 end"),
	%% 2.7.2 transition, after 2.6.8 end
	%% 11/40 interpolation from 30000 to 61440
	?assertEqual(38646,
		big_pricing:get_price_per_gib_minute(26, B),
		"2.7.2 transition price, after 2.6.8 end"),
	%% 2.7.2 end
	?assertEqual(V2Price,
		big_pricing:get_price_per_gib_minute(55, B),
		"2.7.2 end price"),
	%% v2 price
	?assertEqual(V2Price,
		big_pricing:get_price_per_gib_minute(56, B),
		"After 2.7.2 transition end").

test_v2_price() ->
	AtTransitionEnd = big_pricing_transition:transition_start_2_7_2() + 
		big_pricing_transition:transition_length(big_pricing_transition:transition_start_2_7_2()),

	%% 2 chunks per partition when running tests
	%% If we get 1 solution per chunk (or 2 per partition), then we expect a price of 61440
	%%   that's our "baseline" for the purposes of this explanation
	%% AllOneChunkBaseline: 1x baseline
	%%   - 3 1-chunk blocks, 0 2-chunk blocks
	%%   => 3/3 solutions per chunk
	%%   => 2 per partition
	%% AllTwoChunkBaseline: 2x baseline
	%%   - 0 1-chunk blocks, 3 2-chunk blocks
	%%   => max, 2 solutions per chunk
	%%   => 4 per partition
	%% MixedChunkBaseline: 1.5x baseline
	%%   - 2 1-chunk blocks, 1 2-chunk blocks
	%%   => 3/2 solutions per chunk
	%%   => 3 per partition
	do_price_per_gib_minute_post_transition(AtTransitionEnd, 61440, 122880, 92160),
	BeyondTransition = AtTransitionEnd + 1000,
	do_price_per_gib_minute_post_transition(BeyondTransition, 61440, 122880, 92160).

test_v2_price_with_poa1_diff_multiplier() ->
	AtTransitionEnd = big_pricing_transition:transition_start_2_7_2() + 
		big_pricing_transition:transition_length(big_pricing_transition:transition_start_2_7_2()),

	%% 2 chunks per partition when running tests
	%% If we get 1 solution per chunk (or 2 per partition), then we expect a price of 61440
	%%   that's our "baseline" for the purposes of this explanation
	%%
	%% Note: in these tests teh poa1 difficulty modifier is set to 2, which changes the 
	%%       number of solutions per chunk.
	%%
	%% AllOneChunkBaseline: 0.5x baseline
	%%   - 3 1-chunk blocks, 0 2-chunk blocks
	%%   => 3/(3*2) solutions per chunk
	%%   => 1 per partition
	%% AllTwoChunkBaseline: 1.5x baseline
	%%   - 0 1-chunk blocks, 3 2-chunk blocks
	%%   => max, (2+1)/2 solutions per chunk
	%%   => 3 per partition
	%% MixedChunkBaseline: 0.5x baseline
	%%   - 2 1-chunk blocks, 1 2-chunk blocks
	%%   => 3/4 solutions per chunk 
	%%   => 1.5 per partition
	%%   => Since we deal in integers, that gets rounded to 1 per partition
	do_price_per_gib_minute_post_transition(AtTransitionEnd, 30720, 92160, 30720),
	BeyondTransition = AtTransitionEnd + 1000,
	do_price_per_gib_minute_post_transition(BeyondTransition, 30720, 92160, 30720).

do_price_per_gib_minute_post_transition(Height,
		AllOneChunkBaseline, AllTwoChunkBaseline, MixedChunkBaseline) ->
	
	PoA1DiffMultiplier = big_difficulty:poa1_diff_multiplier(Height),
	B0 = #block{
		reward_history = reward_history(1, 1), block_time_history = block_time_history(1, 1) },
	?assertEqual(AllOneChunkBaseline,
		big_pricing:get_price_per_gib_minute(Height, B0),
		io_lib:format(
			"hash_rate: low, reward: low, vdf: perfect, chunks: all_one, poa1_diff: ~B",
			[PoA1DiffMultiplier])),
	B1 = #block{
		reward_history = reward_history(1, 10), block_time_history = block_time_history(1, 1) },
	?assertEqual(AllOneChunkBaseline * 10,
		big_pricing:get_price_per_gib_minute(Height, B1),
		io_lib:format(
			"hash_rate: low, reward: high, vdf: perfect, chunks: all_one, poa1_diff: ~B",
			[PoA1DiffMultiplier])),
	B2 = #block{
		reward_history = reward_history(10, 1), block_time_history = block_time_history(1, 1) },
	?assertEqual(AllOneChunkBaseline div 10,
		big_pricing:get_price_per_gib_minute(Height, B2),
		io_lib:format(
			"hash_rate: high, reward: low, vdf: perfect, chunks: all_one, poa1_diff: ~B",
			[PoA1DiffMultiplier])),
	B3 = #block{
		reward_history = reward_history(10, 10), block_time_history = block_time_history(1, 1) },
	?assertEqual(AllOneChunkBaseline,
		big_pricing:get_price_per_gib_minute(Height, B3),
		io_lib:format(
			"hash_rate: high, reward: high, vdf: perfect, chunks: all_one, poa1_diff: ~B",
			[PoA1DiffMultiplier])),
	B4 = #block{
		reward_history = reward_history(1, 1), block_time_history = block_time_history(10, 1) },
	?assertEqual(AllOneChunkBaseline div 10,
		big_pricing:get_price_per_gib_minute(Height, B4),
		io_lib:format(
			"hash_rate: low, reward: low, vdf: slow, chunks: all_one, poa1_diff: ~B",
			[PoA1DiffMultiplier])),
	B5 = #block{
		reward_history = reward_history(1, 1), block_time_history = block_time_history(1, 10) },
	?assertEqual(AllOneChunkBaseline * 10,
		big_pricing:get_price_per_gib_minute(Height, B5),
		io_lib:format(
			"hash_rate: low, reward: low, vdf: fast, chunks: all_one, poa1_diff: ~B",
			[PoA1DiffMultiplier])),
	B6 = #block{
		reward_history = reward_history(1, 1), block_time_history = all_two_chunks() },
	?assertEqual(AllTwoChunkBaseline,
		big_pricing:get_price_per_gib_minute(Height, B6),
		io_lib:format(
			"hash_rate: low, reward: low, vdf: perfect, chunks: all_two, poa1_diff: ~B",
			[PoA1DiffMultiplier])),
	B7 = #block{
		reward_history = reward_history(1, 1), block_time_history = mix_chunks() },
	?assertEqual(MixedChunkBaseline,
		big_pricing:get_price_per_gib_minute(Height, B7),
		io_lib:format(
			"hash_rate: low, reward: low, vdf: perfect, chunks: mix, poa1_diff: ~B",
			[PoA1DiffMultiplier])).

reward_history(HashRate, Reward) ->
	[
		{crypto:strong_rand_bytes(32), 1*HashRate, 1*Reward, 0},
		{crypto:strong_rand_bytes(32), 2*HashRate, 2*Reward, 0},
		{crypto:strong_rand_bytes(32), 3*HashRate, 3*Reward, 0}
	].

block_time_history(BlockInterval, VDFSteps) ->
	[
		{BlockInterval*10, VDFSteps*10, 1},
		{BlockInterval*20, VDFSteps*20, 1},
		{BlockInterval*30, VDFSteps*30, 1}
	].

all_two_chunks() ->
	[
		{10, 10, 2},
		{20, 20, 2},
		{30, 30, 2}
	].

mix_chunks() ->
	[
		{10, 10, 1},
		{20, 20, 2},
		{30, 30, 1}
	].

recalculate_price_per_gib_minute_test_block() ->
	#block{
		height = ?PRICE_ADJUSTMENT_FREQUENCY-1,
		denomination = 1,
		reward_history = [
			{<<>>, 10000, 10, 1}
		],
		block_time_history = [
			{129, 135, 1}
		],
		price_per_gib_minute = 10000,
		scheduled_price_per_gib_minute = 15000
	}.

recalculate_price_per_gib_minute_2_7_test_() ->
	big_test_node:test_with_mocked_functions(
		[{big_fork, height_2_6, fun() -> -1 end},
		{big_fork, height_2_7, fun() -> -1 end},
		{big_fork, height_2_7_1, fun() -> infinity end}],
		fun() ->
			B = recalculate_price_per_gib_minute_test_block(),
			?assertEqual({15000, 8162}, big_pricing:recalculate_price_per_gib_minute(B)),
			ok
		end).

recalculate_price_per_gib_minute_2_7_1_ema_test_() ->
	big_test_node:test_with_mocked_functions(
		[{big_fork, height_2_6, fun() -> -1 end},
		{big_fork, height_2_7, fun() -> -1 end},
		{big_fork, height_2_7_1, fun() -> -1 end}],
		fun() ->
			B = recalculate_price_per_gib_minute_test_block(),
			?assertEqual({15000, 14316}, big_pricing:recalculate_price_per_gib_minute(B)),
			ok
		end).

auto_redenomination_and_endowment_debt_test_() ->
	%% Set some weird mocks to preserve the existing behavior of this test
	big_test_node:test_with_mocked_functions([
			{big_pricing_transition, transition_start_2_7_2, fun() -> 3 end},
			{big_pricing_transition, transition_length_2_7_2, fun() -> 1 end}
		],
		fun test_auto_redenomination_and_endowment_debt/0).

test_auto_redenomination_and_endowment_debt() ->
	Key1 = {_, Pub1} = big_wallet:new(),
	{_, Pub2} = big_wallet:new(),
	Key3 = {_, Pub3} = big_wallet:new(),
	[B0] = ar_weave:init([
		{big_wallet:to_address(Pub1), 20000000000000, <<>>},
		{big_wallet:to_address(Pub2), 2000000000, <<>>},
		{big_wallet:to_address(Pub3), ?BIG(1000000000000000000), <<>>}
	]),
	big_test_node:start(B0),
	big_test_node:start_peer(peer1, B0),
	big_test_node:connect_to_peer(peer1),
	?assert(big_pricing_transition:transition_start_2_6_8() == 2),
	?assert(big_pricing_transition:transition_length_2_6_8() == 2),
	?assert(?PRICE_ADJUSTMENT_FREQUENCY == 2),
	?assert(?REDENOMINATION_DELAY_BLOCKS == 2),
	?assert(?REWARD_HISTORY_BLOCKS == 3),
	?assert(?DOUBLE_SIGNING_REWARD_SAMPLE_SIZE == 2),
	?assertEqual(262144 * 3, B0#block.weave_size),
	{ok, Config} = application:get_env(bigfile, config),
	{_, MinerPub} = big_wallet:load_key(Config#config.mining_addr),
	?assertEqual(0, get_balance(MinerPub)),
	?assertEqual(0, get_reserved_balance(Config#config.mining_addr)),
	big_test_node:mine(),
	_BI1 = big_test_node:wait_until_height(main, 1),
	big_test_node:assert_wait_until_height(peer1, 1),
	?assertEqual(0, get_balance(MinerPub)),
	B1 = big_node:get_current_block(),
	?assertEqual(10, B1#block.reward),
	?assertEqual(10, get_reserved_balance(B1#block.reward_addr)),
	?assertEqual(1, big_difficulty:get_hash_rate_fixed_ratio(B1)),
	MinerAddr = big_wallet:to_address(MinerPub),
	?assertEqual([{MinerAddr, 1, 10, 1}, {B0#block.reward_addr, 1, 10, 1}],
			B1#block.reward_history),
	?assertEqual([{time_diff(B1, B0), vdf_diff(B1, B0), chunk_count(B1)}, {1, 1, 1}],
			B1#block.block_time_history),
	%% The price is recomputed every two blocks.
	?assertEqual(B1#block.price_per_gib_minute, B1#block.scheduled_price_per_gib_minute),
	big_test_node:mine(),
	_BI2 = big_test_node:wait_until_height(main, 2),
	big_test_node:assert_wait_until_height(peer1, 2),
	?assertEqual(0, get_balance(MinerPub)),
	B2 = big_node:get_current_block(),
	?assertEqual(10, B2#block.reward),
	?assertEqual(20, get_reserved_balance(B2#block.reward_addr)),
	?assertEqual(B1#block.price_per_gib_minute, B2#block.price_per_gib_minute),
	?assertEqual(B2#block.scheduled_price_per_gib_minute, B2#block.price_per_gib_minute),
	?assertEqual([{MinerAddr, 1, 10, 1}, {MinerAddr, 1, 10, 1},
			{B0#block.reward_addr, 1, 10, 1}], B2#block.reward_history),
	?assertEqual([{time_diff(B2, B1), vdf_diff(B2, B1), chunk_count(B2)},
			{time_diff(B1, B0), vdf_diff(B1, B0), chunk_count(B1)}, {1, 1, 1}],
			B2#block.block_time_history),
	big_test_node:mine(),
	_BI3 = big_test_node:wait_until_height(main, 3),
	big_test_node:assert_wait_until_height(peer1, 3),
	?assertEqual(0, get_balance(MinerPub)),
	B3 = big_node:get_current_block(),
	?assertEqual(10, B3#block.reward),
	?assertEqual(30, get_reserved_balance(B3#block.reward_addr)),
	?assertEqual(B3#block.price_per_gib_minute, B2#block.price_per_gib_minute),
	?assertEqual(B3#block.scheduled_price_per_gib_minute,
			B2#block.scheduled_price_per_gib_minute),
	?assertEqual(0, B3#block.kryder_plus_rate_multiplier_latch),
	?assertEqual(1, B3#block.kryder_plus_rate_multiplier),
	?assertEqual([{MinerAddr, 1, 10, 1}, {MinerAddr, 1, 10, 1}, {MinerAddr, 1, 10, 1},
			{B0#block.reward_addr, 1, 10, 1}], B3#block.reward_history),
	Fee = big_test_node:get_optimistic_tx_price(main, 1024),
	big_test_node:mine(),
	_BI4 = big_test_node:wait_until_height(main, 4),
	big_test_node:assert_wait_until_height(peer1, 4),
	B4 = big_node:get_current_block(),
	%% We are at the height ?PRICE_2_6_8_TRANSITION_START + ?PRICE_2_6_8_TRANSITION_BLOCKS
	%% so the new algorithm kicks in which estimates the expected block reward and takes
	%% the missing amount from the endowment pool or takes on debt.
	AvgBlockTime4 = big_block_time_history:compute_block_interval(B3),
	ExpectedReward4 = max(big_inflation:calculate(4), B3#block.price_per_gib_minute
			* ?N_REPLICATIONS(B4#block.height)
			* AvgBlockTime4 div 60
			* 3 div (4 * 1024)), % weave_size / GiB
	?assertEqual(ExpectedReward4, B4#block.reward),
	?assertEqual(ExpectedReward4 + 20, get_reserved_balance(B4#block.reward_addr)),
	?assertEqual(10, get_balance(MinerPub)),
	?assertEqual(ExpectedReward4 - 10, B4#block.debt_supply),
	?assertEqual(1, B4#block.kryder_plus_rate_multiplier_latch),
	?assertEqual(2, B4#block.kryder_plus_rate_multiplier),
	?assertEqual(B4#block.price_per_gib_minute, B3#block.scheduled_price_per_gib_minute),
	PricePerGiBMinute3 = B3#block.price_per_gib_minute,
	?assertEqual(max(PricePerGiBMinute3 div 2, min(PricePerGiBMinute3 * 2,
			big_pricing:get_price_per_gib_minute(B3#block.height, B3))),
			B4#block.scheduled_price_per_gib_minute),
	%% The Kryder+ rate multiplier is 2 now so the fees should have doubled.
	?assert(lists:member(big_test_node:get_optimistic_tx_price(main, 1024), [Fee * 2, Fee * 2 + 1])),
	?assertEqual([{MinerAddr, 1, ExpectedReward4, 1}, {MinerAddr, 1, 10, 1},
			{MinerAddr, 1, 10, 1}, {MinerAddr, 1, 10, 1}, {B0#block.reward_addr, 1, 10, 1}],
			B4#block.reward_history),
	?assertEqual(
		big_rewards:reward_history_hash(B4#block.height, B3#block.reward_history_hash,
			[{MinerAddr, 1, ExpectedReward4, 1}, {MinerAddr, 1, 10, 1}, {MinerAddr, 1, 10, 1}]),
		B4#block.reward_history_hash),
	big_test_node:mine(),
	_BI5 = big_test_node:wait_until_height(main, 5),
	big_test_node:assert_wait_until_height(peer1, 5),
	B5 = big_node:get_current_block(),
	?assertEqual(20, get_balance(MinerPub)),
	AvgBlockTime5 = big_block_time_history:compute_block_interval(B4),
	ExpectedReward5 = max(B4#block.price_per_gib_minute
			* ?N_REPLICATIONS(B5#block.height)
			* AvgBlockTime5 div 60
			* 3 div (4 * 1024), % weave_size / GiB
			big_inflation:calculate(5)),
	?assertEqual(ExpectedReward5, B5#block.reward),
	?assertEqual([{MinerAddr, 1, ExpectedReward5, 1}, {MinerAddr, 1, ExpectedReward4, 1},
			{MinerAddr, 1, 10, 1}, {MinerAddr, 1, 10, 1}, {MinerAddr, 1, 10, 1},
			{B0#block.reward_addr, 1, 10, 1}], B5#block.reward_history),
	?assertEqual(
		big_rewards:reward_history_hash(B5#block.height, B4#block.reward_history_hash,
			[{MinerAddr, 1, ExpectedReward5, 1}, {MinerAddr, 1, ExpectedReward4, 1},
				{MinerAddr, 1, 10, 1}]),
		B5#block.reward_history_hash),
	?assertEqual(1, B5#block.kryder_plus_rate_multiplier_latch),
	?assertEqual(2, B5#block.kryder_plus_rate_multiplier),
	%% The price per GiB minute recalculation only happens every two blocks.
	?assertEqual(B5#block.scheduled_price_per_gib_minute,
			B4#block.scheduled_price_per_gib_minute),
	?assertEqual(B4#block.price_per_gib_minute, B5#block.price_per_gib_minute),
	?assertEqual(20000000000000, get_balance(Pub1)),
	?assert(lists:member(big_test_node:get_optimistic_tx_price(main, 1024), [Fee * 2, Fee * 2 + 1])),
	HalfKryderLatchReset = ?RESET_KRYDER_PLUS_LATCH_THRESHOLD div 2,
	TX1 = big_test_node:sign_tx(main, Key1, #{ denomination => 0, reward => HalfKryderLatchReset }),
	big_test_node:assert_post_tx_to_peer(main, TX1),
	big_test_node:mine(),
	_BI6 = big_test_node:wait_until_height(main, 6),
	big_test_node:assert_wait_until_height(peer1, 6),
	B6 = big_node:get_current_block(),
	{MinerShareDividend, MinerShareDivisor} = ?MINER_FEE_SHARE,
	?assertEqual(30, get_balance(MinerPub)),
	?assertEqual(10 + % inflation
			HalfKryderLatchReset,
			B6#block.reward
			+ B6#block.reward_pool - B5#block.reward_pool
			- (B6#block.debt_supply - B5#block.debt_supply)),
	?assertEqual(20000000000000 - HalfKryderLatchReset, get_balance(Pub1)),
	?assertEqual(1, B6#block.kryder_plus_rate_multiplier_latch),
	?assertEqual(2, B6#block.kryder_plus_rate_multiplier),
	?assertEqual(B6#block.price_per_gib_minute, B5#block.scheduled_price_per_gib_minute),
	ScheduledPricePerGiBMinute5 = B5#block.scheduled_price_per_gib_minute,
	?assertEqual(
		max(ScheduledPricePerGiBMinute5 div 2,
			min(big_pricing:get_price_per_gib_minute(B5#block.height, B5),
				ScheduledPricePerGiBMinute5 * 2)),
			B6#block.scheduled_price_per_gib_minute),
	assert_new_account_fee(),
	?assertEqual(1, B6#block.denomination),
	?assertEqual(?TOTAL_SUPPLY + B6#block.debt_supply - B6#block.reward_pool,
			prometheus_gauge:value(available_supply)),
	?assertEqual([{MinerAddr, 1, B6#block.reward, 1}, {MinerAddr, 1, ExpectedReward5, 1},
			{MinerAddr, 1, ExpectedReward4, 1}], lists:sublist(B6#block.reward_history, 3)),
	TX2 = big_test_node:sign_tx(main, Key1, #{ denomination => 0, reward => HalfKryderLatchReset * 2 }),
	big_test_node:assert_post_tx_to_peer(main, TX2),
	big_test_node:mine(),
	_BI7 = big_test_node:wait_until_height(main, 7),
	big_test_node:assert_wait_until_height(peer1, 7),
	B7 = big_node:get_current_block(),
	?assertEqual(10 + % inflation
			HalfKryderLatchReset * 2,
			B7#block.reward
			+ B7#block.reward_pool - B6#block.reward_pool
			- (B7#block.debt_supply - B6#block.debt_supply)),
	?assertEqual(30 + ExpectedReward4, get_balance(MinerPub)),
	?assertEqual(20000000000000 - HalfKryderLatchReset * 3, get_balance(Pub1)),
	?assert(B7#block.reward_pool > ?RESET_KRYDER_PLUS_LATCH_THRESHOLD),
	?assertEqual(1, B7#block.kryder_plus_rate_multiplier_latch),
	?assertEqual(2, B7#block.kryder_plus_rate_multiplier),
	?assertEqual(B6#block.price_per_gib_minute, B7#block.price_per_gib_minute),
	?assert(big_test_node:get_optimistic_tx_price(main, 1024) > Fee),
	big_test_node:mine(),
	_BI8 = big_test_node:wait_until_height(main, 8),
	big_test_node:assert_wait_until_height(peer1, 8),
	B8 = big_node:get_current_block(),
	?assertEqual(30 + ExpectedReward5 + ExpectedReward4, get_balance(MinerPub)),
	%% Release because at the previous block the endowment pool exceeded the threshold.
	?assert(B8#block.reward_pool < B7#block.reward_pool),
	?assertEqual(0, B8#block.kryder_plus_rate_multiplier_latch),
	?assertEqual(2, B8#block.kryder_plus_rate_multiplier),
	?assertEqual(1, B8#block.denomination),
	?assert(prometheus_gauge:value(available_supply) > ?REDENOMINATION_THRESHOLD),
	?assert(B8#block.scheduled_price_per_gib_minute > B8#block.price_per_gib_minute),
	%% A transaction with explicitly set denomination.
	TX3 = big_test_node:sign_tx(main, Key3, #{ denomination => 1 }),
	{Reward3, _} = big_test_node:get_tx_price(main, 0, big_wallet:to_address(Pub2)),
	{Reward4, _} = big_test_node:get_tx_price(main, 0),
	big_test_node:assert_post_tx_to_peer(main, TX3),
	TX4 = big_test_node:sign_tx(main, Key3, #{ denomination => 2 }),
	?assertMatch({ok, {{<<"400">>, _}, _, _, _, _}}, big_test_node:post_tx_to_peer(main, TX4)),
	?assertEqual({ok, ["invalid_denomination"]}, big_tx_db:get_error_codes(TX4#tx.id)),
	TX5 = big_test_node:sign_tx(main, Key3, #{ denomination => 0, target => big_wallet:to_address(Pub2),
			quantity => 10 }),
	big_test_node:assert_post_tx_to_peer(main, TX5),
	big_test_node:mine(),
	_BI9 = big_test_node:wait_until_height(main, 9),
	big_test_node:assert_wait_until_height(peer1, 9),
	B9 = big_node:get_current_block(),
	?assertEqual(0, B9#block.kryder_plus_rate_multiplier_latch),
	?assertEqual(2, B9#block.kryder_plus_rate_multiplier),
	?assertEqual(1, B9#block.denomination),
	?assertEqual(2, length(B9#block.txs)),
	?assertEqual(0, B9#block.redenomination_height),
	?assert(prometheus_gauge:value(available_supply) < ?REDENOMINATION_THRESHOLD),
	?assertEqual(?BIG(1000000000000000000) - Reward3 - Reward4 - 10, get_balance(Pub3)),
	?assertEqual(2000000000 + 10, get_balance(Pub2)),
	big_test_node:mine(),
	_BI10 = big_test_node:wait_until_height(main, 10),
	big_test_node:assert_wait_until_height(peer1, 10),
	B10 = big_node:get_current_block(),
	?assertEqual(9 + ?REDENOMINATION_DELAY_BLOCKS, B10#block.redenomination_height),
	?assertEqual(1, B10#block.denomination),
	TX6 = big_test_node:sign_tx(main, Key3, #{ denomination => 0 }),
	%% Transactions without explicit denomination are not accepted now until
	%% the redenomination height.
	?assertMatch({ok, {{<<"400">>, _}, _, _, _, _}}, big_test_node:post_tx_to_peer(main, TX6)),
	?assertEqual({ok, ["invalid_denomination"]}, big_tx_db:get_error_codes(TX6#tx.id)),
	%% The redenomination did not start yet.
	TX7 = big_test_node:sign_tx(main, Key3, #{ denomination => 2 }),
	?assertMatch({ok, {{<<"400">>, _}, _, _, _, _}}, big_test_node:post_tx_to_peer(main, TX7)),
	?assertEqual({ok, ["invalid_denomination"]}, big_tx_db:get_error_codes(TX7#tx.id)),
	{_, Pub4} = big_wallet:new(),
	TX8 = big_test_node:sign_tx(main, Key3, #{ denomination => 1, target => big_wallet:to_address(Pub4),
			quantity => 3 }),
	big_test_node:assert_post_tx_to_peer(main, TX8),
	big_test_node:mine(),
	_BI11 = big_test_node:wait_until_height(main, 11),
	big_test_node:assert_wait_until_height(peer1, 11),
	B11 = big_node:get_current_block(),
	?assertEqual(1, length(B11#block.txs)),
	?assertEqual(1, B11#block.denomination),
	?assertEqual(3, get_balance(Pub4)),
	Balance11 = get_balance(Pub3),
	{_, Pub5} = big_wallet:new(),
	TX9 = big_test_node:sign_v1_tx(main, Key3, #{ denomination => 0, target => big_wallet:to_address(Pub5),
			quantity => 100 }),
	?assertMatch({ok, {{<<"400">>, _}, _, _, _, _}}, big_test_node:post_tx_to_peer(main, TX9)),
	?assertEqual({ok, ["invalid_denomination"]}, big_tx_db:get_error_codes(TX9#tx.id)),
	%% The redenomination did not start just yet.
	TX10 = big_test_node:sign_v1_tx(main, Key3, #{ denomination => 2 }),
	?assertMatch({ok, {{<<"400">>, _}, _, _, _, _}}, big_test_node:post_tx_to_peer(main, TX10)),
	?assertEqual({ok, ["invalid_denomination"]}, big_tx_db:get_error_codes(TX10#tx.id)),
	{Reward11, _} = big_test_node:get_tx_price(main, 0, big_wallet:to_address(Pub5)),
	?assert(big_difficulty:get_hash_rate_fixed_ratio(B11) > 1),
	?assertEqual(lists:sublist([{MinerAddr, big_difficulty:get_hash_rate_fixed_ratio(B11), B11#block.reward, 1}
			| B10#block.reward_history],
			?REWARD_HISTORY_BLOCKS + ?STORE_BLOCKS_BEHIND_CURRENT),
			B11#block.reward_history),
	TX11 = big_test_node:sign_tx(main, Key3, #{ denomination => 1, target => big_wallet:to_address(Pub5),
			quantity => 100 }),
	big_test_node:assert_post_tx_to_peer(main, TX11),
	big_test_node:mine(),
	_BI12 = big_test_node:wait_until_height(main, 12),
	big_test_node:assert_wait_until_height(peer1, 12),
	?assertEqual(100 * 1000, get_balance(Pub5)),
	?assertEqual(3 * 1000, get_balance(Pub4)),
	?assertEqual((Balance11 - Reward11 - 100) * 1000, get_balance(Pub3)),
	B12 = big_node:get_current_block(),
	?assertEqual(1, length(B12#block.txs)),
	?assertEqual(2, B12#block.denomination),
	?assertEqual(10 * 1000 + % inflation
			Reward11 * 1000, % fees
			B12#block.reward
			+ B12#block.reward_pool - B11#block.reward_pool * 1000
			- (B12#block.debt_supply - B11#block.debt_supply * 1000)),
	?assertEqual(B11#block.debt_supply * 1000, B12#block.debt_supply),
	%% Setting the price scheduled on height=10.
	?assertEqual(B11#block.scheduled_price_per_gib_minute * 1000,
			B12#block.price_per_gib_minute),
	?assertEqual([{MinerAddr, big_difficulty:get_hash_rate_fixed_ratio(B12), B12#block.reward, 2} | B11#block.reward_history],
			B12#block.reward_history),
	TX12 = big_test_node:sign_tx(main, Key3, #{ denomination => 0, quantity => 10,
			target => big_wallet:to_address(Pub5) }),
	{Reward12, 2} = big_test_node:get_tx_price(main, 0),
	big_test_node:assert_post_tx_to_peer(main, TX12),
	TX13 = big_test_node:sign_v1_tx(main, Key3, #{ denomination => 0,
			reward => big_test_node:get_optimistic_tx_price(main, 0),
			target => big_wallet:to_address(Pub4), quantity => 4 }),
	big_test_node:assert_post_tx_to_peer(main, TX13),
	Reward14 = big_test_node:get_optimistic_tx_price(main, 0),
	TX14 = big_test_node:sign_v1_tx(main, Key3, #{ denomination => 2, reward => Reward14,
			quantity => 5, target => big_wallet:to_address(Pub4) }),
	big_test_node:assert_post_tx_to_peer(main, TX14),
	TX15 = big_test_node:sign_v1_tx(main, Key3, #{ denomination => 2,
			reward => erlang:ceil(Reward14 / 1000) }),
	?assertMatch({ok, {{<<"400">>, _}, _, _, _, _}}, big_test_node:post_tx_to_peer(main, TX15)),
	?assertEqual({ok, ["tx_too_cheap"]}, big_tx_db:get_error_codes(TX15#tx.id)),
	TX16 = big_test_node:sign_v1_tx(main, Key3, #{ denomination => 3 }),
	?assertMatch({ok, {{<<"400">>, _}, _, _, _, _}}, big_test_node:post_tx_to_peer(main, TX16)),
	?assertEqual({ok, ["invalid_denomination"]}, big_tx_db:get_error_codes(TX16#tx.id)),
	{_, Pub6} = big_wallet:new(),
	%% Divide the reward by 1000 and specify the previous denomination.
	Reward17 = big_test_node:get_optimistic_tx_price(main, 0, big_wallet:to_address(Pub6)),
	TX17 = big_test_node:sign_tx(main, Key3, #{ denomination => 1,
			reward => erlang:ceil(Reward17 / 1000), target => big_wallet:to_address(Pub6),
			quantity => 7 }),
	big_test_node:assert_post_tx_to_peer(main, TX17),
	big_test_node:mine(),
	_BI13 = big_test_node:wait_until_height(main, 13),
	big_test_node:assert_wait_until_height(peer1, 13),
	B13 = big_node:get_current_block(),
	Reward17_2 = erlang:ceil(Reward17 / 1000) * 1000,
	AvgBlockTime13 = big_block_time_history:compute_block_interval(B12),
	BaseReward13 =
		B12#block.price_per_gib_minute
		* ?N_REPLICATIONS(B13#block.height)
		* AvgBlockTime13 div 60 % minutes
		* B13#block.weave_size div (1024 * 1024 * 1024), % weave_size / GiB
	FeeSum13 =
			Reward12 * MinerShareDividend div MinerShareDivisor % TX12
			+ Reward14 * MinerShareDividend div MinerShareDivisor % TX13
			+ Reward14 * MinerShareDividend div MinerShareDivisor % TX14
			+ Reward17_2 * MinerShareDividend div MinerShareDivisor, % TX17
	?debugFmt("B12#block.reward_pool: ~B, fees: ~B, fees received by the reward pool:~B, "
			"expected reward: ~B, miner fee share: ~B~n", [B12#block.reward_pool,
				Reward12 + Reward14 * 2 + Reward17_2,
				Reward12 + Reward14 * 2 + Reward17_2 - FeeSum13,
				BaseReward13, FeeSum13]),
	?assertEqual(B12#block.reward_pool + Reward12 + Reward14 * 2 + Reward17_2 - FeeSum13
			- max(0, (BaseReward13 - (10 * 1000 + FeeSum13))), B13#block.reward_pool),
	?assertEqual(4, length(B13#block.txs)),
	?assertEqual(2, B13#block.denomination),
	?assertEqual(100 * 1000 + 10, get_balance(Pub5)),
	?assertEqual(7 * 1000, get_balance(Pub6)),
	?assertEqual(3 * 1000 + 4 + 5, get_balance(Pub4)),
	assert_new_account_fee(),
	big_test_node:mine(),
	_BI14 = big_test_node:wait_until_height(main, 14),
	big_test_node:assert_wait_until_height(peer1, 14),
	B14 = big_node:get_current_block(),
	ScheduledPricePerGiBMinute13 = B13#block.scheduled_price_per_gib_minute,
	?assertEqual(
		max(ScheduledPricePerGiBMinute13 div 2, min(
			big_pricing:get_price_per_gib_minute(B13#block.height, B13),
			ScheduledPricePerGiBMinute13 * 2
		)), B14#block.scheduled_price_per_gib_minute).

time_diff(#block{ timestamp = TS }, #block{ timestamp = PrevTS }) ->
	TS - PrevTS.

vdf_diff(B, PrevB) ->
	big_block:vdf_step_number(B) - big_block:vdf_step_number(PrevB).

chunk_count(#block{ recall_byte2 = undefined }) ->
	1;
chunk_count(_) ->
	2.

assert_new_account_fee() ->
	?assert(big_test_node:get_optimistic_tx_price(main,
			262144 + ?NEW_ACCOUNT_FEE_DATA_SIZE_EQUIVALENT) >
			big_test_node:get_optimistic_tx_price(main, 0, <<"non-existent-address">>)),
	?assert(big_test_node:get_optimistic_tx_price(main,
			?NEW_ACCOUNT_FEE_DATA_SIZE_EQUIVALENT - 262144) <
			big_test_node:get_optimistic_tx_price(main, 0, <<"non-existent-address">>)).

%% @doc Return the current balance of the given account.
get_balance(Pub) ->
	Address = big_wallet:to_address(Pub),
	Peer = big_test_node:peer_ip(main),
	{ok, {{<<"200">>, _}, _, Reply, _, _}} =
		big_http:req(#{
			method => get,
			peer => Peer,
			path => "/wallet/" ++ binary_to_list(ar_util:encode(Address)) ++ "/balance"
		}),
	Balance = binary_to_integer(Reply),
	B = big_node:get_current_block(),
	{ok, {{<<"200">>, _}, _, Reply2, _, _}} =
		big_http:req(#{
			method => get,
			peer => Peer,
			path => "/wallet_list/" ++ binary_to_list(ar_util:encode(B#block.wallet_list))
					++ "/" ++ binary_to_list(ar_util:encode(Address)) ++ "/balance"
		}),
	case binary_to_integer(Reply2) of
		Balance ->
			Balance;
		Balance2 ->
			?assert(false, io_lib:format("Expected: ~B, got: ~B.~n", [Balance, Balance2]))
	end.


get_reserved_balance(Address) ->
	Peer = big_test_node:peer_ip(main),
	{ok, {{<<"200">>, _}, _, Reply, _, _}} =
		big_http:req(#{
			method => get,
			peer => Peer,
			path => "/wallet/" ++ binary_to_list(ar_util:encode(Address))
					++ "/reserved_rewards_total"
		}),
	binary_to_integer(Reply).
