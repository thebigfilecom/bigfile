-module(big_tx_replay_pool_tests).

-include_lib("bigfile/include/big.hrl").
-include_lib("bigfile/include/big_pricing.hrl").

-include_lib("eunit/include/eunit.hrl").

verify_block_txs_test_() ->
	{timeout, 30, fun test_verify_block_txs/0}.

test_verify_block_txs() ->
	Key1 = big_wallet:new(),
	Key2 = big_wallet:new(),
	RandomBlockAnchors =
		[crypto:strong_rand_bytes(32) || _ <- lists:seq(1, ?MAX_TX_ANCHOR_DEPTH)],
	BlockAnchorTXAtForkHeight = tx(Key1, fee(big_fork:height_2_0()), <<"hash">>),
	BlockAnchorTXAfterForkHeight =
		tx(Key1, fee(big_fork:height_2_0() + 1), <<"hash">>),
	Timestamp = os:system_time(second),
	TestCases = [
		#{
			title => "Fork height 2.0 accepts block anchors",
			txs => [tx(Key1, fee(big_fork:height_2_0()), <<"hash">>)],
			height => big_fork:height_2_0(),
			block_anchors => [<<"hash">>],
			recent_txs_map => #{},
			wallet_list => [wallet(Key1, fee(big_fork:height_2_0()))],
			expected_result => valid
		},
		#{
			title => "After fork height 2.0 accepts block anchors",
			txs => [tx(Key1, fee(big_fork:height_2_0() + 1), <<"hash">>)],
			height => big_fork:height_2_0() + 1,
			block_anchors => [<<"hash">>],
			recent_txs_map => #{},
			wallet_list => [wallet(Key1, fee(big_fork:height_2_0() + 1))],
			expected_result => valid
		},
		#{
			title => "Fork height 2.0 rejects outdated block anchors",
			txs => [
				tx(
					Key1,
					fee(big_fork:height_2_0()),
					crypto:strong_rand_bytes(32)
				)
			],
			block_anchors => RandomBlockAnchors,
			recent_txs_map => #{},
			height => big_fork:height_2_0(),
			wallet_list => [wallet(Key1, fee(big_fork:height_2_0()))],
			expected_result => invalid
		},
		#{
			title => "Fork height 2.0 accepts wallet list anchors",
			txs => [
				tx(Key1, fee(big_fork:height_2_0()), <<>>),
				tx(Key2, fee(big_fork:height_2_0()), <<>>)
			],
			height => big_fork:height_2_0(),
			wallet_list => [
				wallet(Key1, fee(big_fork:height_2_0())),
				wallet(Key2, fee(big_fork:height_2_0()))
			],
			block_anchors => [],
			recent_txs_map => #{},
			expected_result => valid
		},
		#{
			title => "After fork height 2.0 accepts wallet list anchors",
			txs => [
				tx(Key1, fee(big_fork:height_2_0() + 1), <<>>),
				tx(Key2, fee(big_fork:height_2_0() + 1), <<>>)
			],
			height => big_fork:height_2_0() + 1,
			wallet_list => [
				wallet(Key1, fee(big_fork:height_2_0() + 1)),
				wallet(Key2, fee(big_fork:height_2_0() + 1))
			],
			block_anchors => [],
			recent_txs_map => #{},
			expected_result => valid
		},
		#{
			title => "Fork height 2.0 rejects conflicting wallet list anchors",
			txs => [
				tx(Key1, fee(big_fork:height_2_0()), <<>>),
				tx(Key1, fee(big_fork:height_2_0()), <<>>)
			],
			height => big_fork:height_2_0(),
			block_anchors => [],
			recent_txs_map => #{},
			wallet_list => [wallet(Key1, 2 * fee(big_fork:height_2_0()))],
			expected_result => invalid
		},
		#{
			title => "Fork height 2.0 rejects chained wallet list anchors",
			txs => make_tx_chain(Key1, big_fork:height_2_0()),
			height => big_fork:height_2_0(),
			block_anchors => [],
			recent_txs_map => #{},
			wallet_list => [wallet(Key1, 2 * fee(big_fork:height_2_0()))],
			expected_result => invalid
		},
		#{
			title => "Fork height 2.0 rejects conflicting balances",
			txs => [
				tx(Key1, fee(big_fork:height_2_0()), <<>>),
				tx(Key1, fee(big_fork:height_2_0()), <<>>)
			],
			height => big_fork:height_2_0(),
			wallet_list =>
				[wallet(Key1, erlang:trunc(1.5 * fee(big_fork:height_2_0())))],
			block_anchors => [],
			recent_txs_map => #{},
			expected_result => invalid
		},
		#{
			title => "Fork height 2.0 rejects duplicates",
			txs => [BlockAnchorTXAtForkHeight, BlockAnchorTXAtForkHeight],
			height => big_fork:height_2_0(),
			block_anchors => [],
			recent_txs_map => #{},
			wallet_list => [wallet(Key1, 2 * fee(big_fork:height_2_0()))],
			expected_result => invalid
		},
		#{
			title => "After fork height 2.0 rejects duplicates",
			txs => [BlockAnchorTXAfterForkHeight, BlockAnchorTXAfterForkHeight],
			height => big_fork:height_2_0() + 1,
			block_anchors => [],
			recent_txs_map => #{},
			wallet_list => [wallet(Key1, 2 * fee(big_fork:height_2_0() + 1))],
			expected_result => invalid
		},
		#{
			title => "Fork height 2.0 rejects txs from the weave",
			txs => [BlockAnchorTXAtForkHeight],
			height => big_fork:height_2_0(),
			block_anchors => [<<"hash">>, <<"otherhash">>],
			recent_txs_map => #{
				<<"txid">> => ok,
				<<"txid2">> => ok,
				BlockAnchorTXAtForkHeight#tx.id => ok
			},
			wallet_list => [wallet(Key1, fee(big_fork:height_2_0()))],
			expected_result => invalid
		},
		#{
			title => "After fork height 2.0 rejects txs from the weave",
			txs => [BlockAnchorTXAfterForkHeight],
			height => big_fork:height_2_0() + 1,
			block_anchors => [<<"hash">>, <<"otherhash">>],
			recent_txs_map => #{
				<<"txid">> => ok,
				<<"txid2">> => ok,
				BlockAnchorTXAfterForkHeight#tx.id => ok
			},
			wallet_list => [wallet(Key1, fee(big_fork:height_2_0() + 1))],
			expected_result => invalid
		}
	],
	lists:foreach(
		fun(#{
			title := Title,
			txs := TXs,
			height := Height,
			wallet_list := WL,
			block_anchors := BlockAnchors,
			recent_txs_map := RecentTXMap,
			expected_result := ExpectedResult
		}) ->
			Rate = {1, 4},
			PricePerGiBMinute = 2000,
			KryderPlusRateMultiplier = 1,
			Denomination = 1,
			RedenominationHeight = 0,
			Wallets = maps:from_list([{A, {B, LTX}} || {A, B, LTX} <- WL]),
			?debugFmt("~s:~n", [Title]),
			?assertEqual(
				ExpectedResult,
				big_tx_replay_pool:verify_block_txs({TXs, Rate, PricePerGiBMinute,
						KryderPlusRateMultiplier, Denomination, Height, RedenominationHeight,
						Timestamp, Wallets, BlockAnchors, RecentTXMap}),
				Title),
			PickedTXs = big_tx_replay_pool:pick_txs_to_mine({BlockAnchors, RecentTXMap, Height,
					RedenominationHeight, Rate, PricePerGiBMinute, KryderPlusRateMultiplier,
					Denomination, Timestamp, Wallets, TXs}),
			?assertEqual(
				valid,
				big_tx_replay_pool:verify_block_txs({PickedTXs, Rate, PricePerGiBMinute,
						KryderPlusRateMultiplier, Denomination, Height, RedenominationHeight,
						Timestamp, Wallets, BlockAnchors, RecentTXMap}),
				lists:flatten(
					io_lib:format("Verifying after picking_txs_to_mine: ~s:", [Title])
				)
			)
		end,
		TestCases
	).

make_tx_chain(Key, Height) ->
	TX1 = tx(Key, fee(Height), <<>>),
	TX2 = tx(Key, fee(Height), TX1#tx.id),
	[TX1, TX2].

tx(Key = {_, {_, Owner}}, Reward, Anchor) ->
	big_tx:sign(
		#tx{
			format = 2,
			owner = Owner,
			reward = Reward,
			last_tx = Anchor
		},
		Key
	).

wallet({_, Pub}, Balance) ->
	{big_wallet:to_address(Pub), Balance, <<>>}.

fee(Height) ->
	big_tx:get_tx_fee({0, 2000, 1, <<>>, #{}, Height + 1}).
