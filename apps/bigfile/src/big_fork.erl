%%%
%%% @doc The module defines BigFile hard forks' heights.
%%%

-module(big_fork).

-export([height_1_6/0, height_1_7/0, height_1_8/0, height_1_9/0, height_2_0/0, height_2_2/0,
		height_2_3/0, height_2_4/0, height_2_5/0, height_2_6/0, height_2_6_8/0,
		height_2_7/0, height_2_7_1/0, height_2_7_2/0,
		height_2_8/0]).

-include_lib("bigfile/include/big.hrl").
-include_lib("bigfile/include/big_consensus.hrl").

-ifdef(FORKS_RESET).
height_1_6() ->
	0.
-else.
height_1_6() ->
	0.
-endif.

-ifdef(FORKS_RESET).
height_1_7() ->
	0.
-else.
height_1_7() ->
	0. % Targeting 2019-07-08 UTC
-endif.

-ifdef(FORKS_RESET).
height_1_8() ->
	0.
-else.
height_1_8() ->
	0. % Targeting 2019-08-29 UTC
-endif.

-ifdef(FORKS_RESET).
height_1_9() ->
	0.
-else.
height_1_9() ->
	0. % Targeting 2019-11-04 UTC
-endif.

-ifdef(FORKS_RESET).
height_2_0() ->
	0.
-else.
height_2_0() ->
	0. % Targeting 2020-04-09 10:00 UTC
-endif.

-ifdef(FORKS_RESET).
height_2_2() ->
	0.
-else.
height_2_2() ->
	0. % Targeting 2020-10-21 13:00 UTC
-endif.

-ifdef(FORKS_RESET).
height_2_3() ->
	0.
-else.
height_2_3() ->
	0. % Targeting 2020-12-21 11:00 UTC
-endif.

-ifdef(FORKS_RESET).
height_2_4() ->
	0.
-else.
height_2_4() ->
	0. % Targeting 2021-02-24 11:50 UTC
-endif.

-ifdef(FORKS_RESET).
height_2_5() ->
	0.
-else.
height_2_5() ->
	0.
-endif.

-ifdef(FORK_2_6_HEIGHT).
height_2_6() ->
	?FORK_2_6_HEIGHT.
-else.
	-ifdef(FORKS_RESET).
		height_2_6() ->
			0.
	-else.
		height_2_6() ->
			0. % Targeting 2023-03-06 14:00 UTC
	-endif.
-endif.

-ifdef(FORK_2_6_8_HEIGHT).
height_2_6_8() ->
	?FORK_2_6_8_HEIGHT.
-else.
	-ifdef(FORKS_RESET).
		height_2_6_8() ->
			0.
	-else.
		height_2_6_8() ->
			0. % Targeting 2023-05-30 16:00 UTC
	-endif.
-endif.

-ifdef(FORK_2_7_HEIGHT).
height_2_7() ->
	?FORK_2_7_HEIGHT.
-else.
	-ifdef(FORKS_RESET).
		height_2_7() ->
			0.
	-else.
		height_2_7() ->
			0. % Targeting 2023-10-04 14:00 UTC
	-endif.
-endif.

-ifdef(FORK_2_7_1_HEIGHT).
height_2_7_1() ->
	?FORK_2_7_1_HEIGHT.
-else.
	-ifdef(FORKS_RESET).
		height_2_7_1() ->
			0.
	-else.
		height_2_7_1() ->
			0. % Targeting 2023-12-05 14:00 UTC
	-endif.
-endif.

-ifdef(FORK_2_7_2_HEIGHT).
height_2_7_2() ->
	?FORK_2_7_2_HEIGHT.
-else.
	-ifdef(FORKS_RESET).
		height_2_7_2() ->
			0.
	-else.
		height_2_7_2() ->
			0. % Targeting 2024-03-26 14:00 UTC
	-endif.
-endif.

-ifdef(FORK_2_8_HEIGHT).
height_2_8() ->
	?FORK_2_8_HEIGHT.
-else.
	-ifdef(FORKS_RESET).
		height_2_8() ->
			0.
	-else.
		height_2_8() ->
			0. % Targeting 2024-11-13 14:00 UTC
	-endif.
-endif.
