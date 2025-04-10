-module(big_poller_tests).

-include_lib("bigfile/include/big.hrl").
-include_lib("bigfile/include/big_config.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(big_test_node, [assert_wait_until_height/2, read_block_when_stored/1]).

polling_test_() ->
	{timeout, 120, fun test_polling/0}.

test_polling() ->
	{_, Pub} = Wallet = big_wallet:new(),
	[B0] = ar_weave:init([{big_wallet:to_address(Pub), ?BIG(10000), <<>>}]),
	big_test_node:start(B0),
	big_test_node:start_peer(peer1, B0),
	big_test_node:disconnect_from(peer1),
	TXs =
		lists:map(
			fun(Height) ->
				SignedTX = big_test_node:sign_tx(Wallet, #{ last_tx => big_test_node:get_tx_anchor(peer1) }),
				big_test_node:assert_post_tx_to_peer(peer1, SignedTX),
				big_test_node:mine(peer1),
				assert_wait_until_height(peer1, Height),
				SignedTX
			end,
			lists:seq(1, 9)
		),
	big_test_node:connect_to_peer(peer1),
	big_test_node:wait_until_height(main, 9),
	lists:foreach(
		fun(Height) ->
			{H, _, _} = big_node:get_block_index_entry(Height),
			B = read_block_when_stored(H),
			TX = lists:nth(Height, TXs),
			?assertEqual([TX#tx.id], B#block.txs)
		end,
		lists:seq(1, 9)
	),
	%% Make the nodes diverge. Expect one of them to fetch and apply the blocks
	%% from the winning fork.
	big_test_node:disconnect_from(peer1),
	big_test_node:mine(),
	big_test_node:mine(peer1),
	[{MH11, _, _} | _] = big_test_node:wait_until_height(main, 10),
	[{SH11, _, _} | _] = big_test_node:wait_until_height(peer1, 10),
	?assertNotEqual(SH11, MH11),
	big_test_node:mine(),
	big_test_node:mine(peer1),
	[{MH12, _, _} | _] = big_test_node:wait_until_height(main, 11),
	[{SH12, _, _} | _] = big_test_node:wait_until_height(peer1, 11),
	?assertNotEqual(SH12, MH12),
	big_test_node:mine(),
	big_test_node:mine(peer1),
	[{MH13, _, _} | _] = MBI12 = big_test_node:wait_until_height(main, 12),
	[{SH13, _, _} | _] = SBI12 = big_test_node:wait_until_height(peer1, 12),
	?assertNotEqual(SH13, MH13),
	BM13 = big_block_cache:get(block_cache, MH13),
	BS13 = big_test_node:remote_call(peer1, big_block_cache, get, [block_cache, SH13]),
	CDiffM13 = BM13#block.cumulative_diff,
	CDiffS13 = BS13#block.cumulative_diff,
	big_test_node:connect_to_peer(peer1),
	case CDiffM13 > CDiffS13 of
		true ->
			?debugFmt("Case 1.", []),
			?assertEqual(ok, big_test_node:wait_until_block_index(peer1, MBI12)),
			?assertMatch([{MH13, _, _} | _], big_node:get_block_index());
		false ->
			case CDiffM13 < CDiffS13 of
				true ->
					?debugFmt("Case 2.", []),
					?assertEqual(ok, big_test_node:wait_until_block_index(SBI12)),
					?assertMatch([{SH13, _, _} | _],
							big_test_node:remote_call(peer1, big_node, get_block_index, []));
				false ->
					?debugFmt("Case 3.", []),
					big_test_node:mine(peer1),
					[{MH14, _, _}, {MH13_1, _, _}, {MH12_1, _, _}, {MH11_1, _, _} | _]
					= big_test_node:wait_until_height(main, 13),
					[{SH14, _, _} | _] = big_test_node:wait_until_height(peer1, 13),
					?assertEqual(MH14, SH14),
					?assertEqual(SH13, MH13_1),
					?assertEqual(SH12, MH12_1),
					?assertEqual(SH11, MH11_1)
			end
	end.
