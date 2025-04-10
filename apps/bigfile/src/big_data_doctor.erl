-module(big_data_doctor).

-export([main/0, main/1]).

-include_lib("bigfile/include/big.hrl").
-include_lib("bigfile/include/big_config.hrl").
-include_lib("bigfile/include/big_chunk_storage.hrl").
-include_lib("bigfile/include/big_consensus.hrl").

main() ->
	main([]).
main([]) ->
	help(),
	erlang:halt(1);
main(Args) ->
	logger:set_handler_config(default, level, error),
	Command = hd(Args),
	Success = case Command of
		"merge" ->
			big_doctor_merge:main(tl(Args));
		"bench" ->
			big_doctor_bench:main(tl(Args));
		"dump" ->
			big_doctor_dump:main(tl(Args));
		_ ->
			false
	end,
	case Success of
		true ->
			erlang:halt(0);
		_ ->
			help(),
			erlang:halt(1)
	end.

help() ->
	big:console("~n"),
	big_doctor_merge:help(),
	big:console("~n"),
	big_doctor_bench:help(),
	big:console("~n"),
	big_doctor_dump:help(),
	big:console("~n").

