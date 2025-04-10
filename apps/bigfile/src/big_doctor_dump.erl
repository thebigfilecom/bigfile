-module(big_doctor_dump).

-export([main/1, help/0]).

-include_lib("kernel/include/file.hrl").
-include_lib("bigfile/include/big.hrl").
-include_lib("bigfile/include/big_config.hrl").

main(Args) ->
	dump(Args).

help() ->
	big:console("data-doctor dump <include_txs> <block_id> <min_height> <data_dir> <output_dir>~n"),
	big:console("  include_txs: Whether to include transactions in the dump (true/false).~n"),
	big:console("  block_id: The block ID to start the dump from.~n"),
	big:console("  min_height: The minimum height of the blocks to dump.~n"),
	big:console("  data_dir: Full path to your data_dir.~n"),
	big:console("  output_dir: Full path to a directory where the dumped data will be written.~n"),
	big:console("~nExample:~n"),
	big:console("data-doctor dump true ZR7zbobdw55a....pRpUabEkLD0V 100000 /mnt/bigfile-data /mnt/output~n").

dump([IncludeTXs, H, MinHeight, DataDir, OutputDir]) ->
	ok = filelib:ensure_dir(filename:join([OutputDir, "blocks", "dummy"])),
	ok = filelib:ensure_dir(filename:join([OutputDir, "txs", "dummy"])),

	Config = #config{data_dir = DataDir},
	application:set_env(bigfile, config, Config),
	big_kv_sup:start_link(),
	big_storage_sup:start_link(),

	dump_blocks(ar_util:decode(H),
		list_to_integer(MinHeight),
		OutputDir,
		list_to_boolean(IncludeTXs)),
	true;
dump(_) ->
	false.

list_to_boolean("true") -> true;
list_to_boolean("false") -> false;
list_to_boolean(_) -> false.

dump_blocks(BH, MinHeight, OutputDir, IncludeTXs) ->
	H = ar_util:encode(BH),
	case big_kv:get(block_db, BH) of
		{ok, Bin} ->
			try
				case big_serialize:binary_to_block(Bin) of
					{ok, B} ->
						case B#block.height >= MinHeight of
							true ->
								io:format("Block: ~p / ~p", [B#block.height, H]),
								JsonFilename = io_lib:format("~s.json", [ar_util:encode(B#block.indep_hash)]),
								OutputFilePath = filename:join([OutputDir, "blocks", JsonFilename]),
								case file:read_file_info(OutputFilePath) of
									{ok, _FileInfo} ->
										io:format(" ... skipping~n"),
										ok; % File exists, do nothing
									{error, enoent} ->
										io:format(" ... writing~n"),
										% File does not exist, proceed with processing
										case IncludeTXs of
											true ->
												dump_txs(B#block.txs, OutputDir);
											false ->
												ok
										end,
										Json = big_serialize:block_to_json_struct(B),
										JsonString = big_serialize:jsonify(Json),
										file:write_file(OutputFilePath, JsonString)
								end,
								PrevBH = B#block.previous_block,
								dump_blocks(PrevBH, MinHeight, OutputDir, IncludeTXs);
							false ->
								io:format("Done.~n")
						end;
					_ ->
						ok
				end
			catch
				Type:Reason ->
					io:format("Error processing block ~p: ~p:~p~n", [H, Type, Reason])
			end;
        not_found ->
            io:format("Block ~p not found.~n", [H])
    end.

dump_txs([], _OutputDir) ->
	ok;
dump_txs([TXID | TXIDs], OutputDir) ->
	case big_kv:get(tx_db, TXID) of
		{ok, Bin} ->
			{ok, TX} = big_serialize:binary_to_tx(Bin),
			Json = big_serialize:tx_to_json_struct(TX),
			JsonString = big_serialize:jsonify(Json),
			JsonFilename = io_lib:format("~s.json", [ar_util:encode(TXID)]),
			OutputFilePath = filename:join([OutputDir, "txs", JsonFilename]),
			file:write_file(OutputFilePath, JsonString);
		_ ->
			ok
	end,
	dump_txs(TXIDs, OutputDir).
