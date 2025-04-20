-module(big_p3_config_tests).

-include_lib("bigfile/include/big.hrl").
-include_lib("bigfile/include/big_config.hrl").
-include_lib("bigfile/include/big_p3.hrl").

-include_lib("eunit/include/eunit.hrl").

-import(big_p3_tests, [raw_request/2, http_request/1]).

-export([
	sample_p3_config/0, sample_p3_config/1, sample_p3_config/3, sample_p3_config/4,
	empty_p3_config/0]).

-define (DEPOSIT_ADDRESS, "BHAWuomQUIL18WON2LjqjDF4YuRDcmhme7wvFW2BDiU").
-define (DEPOSIT_ADDRESS_CHECKSUM, "ToOiTg").

%% @doc The XXX_parse_test() tests assert that a correctly formatted p3 configuration block is
%% correctly parsed. They do *not* validate that the configuration data is semantically
%% correct - that level of business logic validation is tested in the
%% XXX_validate_test() tests.
%%
%% In addition to testing the parsing functionality, these tests also assert that the
%% helper functions (like sample_p3_config/0) return valid services config data.
%% They do this by comparing the return of the helper functions to the output of the JSON
%% parsing functions. Since these helper functions are used by other tests
%% (e.g. big_p3_tests.erl), it's important that they always returns the current and valid
%% in-memory representation of a P3 services configuration.
empty_config_parse_test() ->
	Config = <<"{}">>,
	{ok, ParsedConfig} = big_config:parse(Config),
	ExpectedConfig = empty_p3_config(),
	?assertEqual(ExpectedConfig, ParsedConfig#config.p3).

no_p3_parse_test() ->
	Config = <<"{\"p3\": {}}">>,
	{ok, ParsedConfig} = big_config:parse(Config),
	ExpectedConfig = empty_p3_config(),
	?assertEqual(ExpectedConfig, ParsedConfig#config.p3).

empty_p3_parse_test() ->
	Config = <<"{\"p3\": { \"payments\": {}, \"services\": [] }}">>,
	{ok, ParsedConfig} = big_config:parse(Config),
	ExpectedConfig = empty_p3_config(),
	?assertEqual(ExpectedConfig, ParsedConfig#config.p3).

basic_parse_test() ->
	Config = <<"{
		\"p3\": {
			\"payments\": {
				\"bigfile/BIG\": {
					\"address\": \"gCmvhZDHpVFKQ7spt8XHxe5FGtJSxcIHxcNpBFKt9NI\",
					\"minimum_balance\": \"-1000000\",
					\"confirmations\": 2
				}
			},
			\"services\": [
				{
					\"endpoint\": \"/price/{bytes}\",
					\"mod_seq\": 1,
					\"rate_type\": \"request\",
					\"rates\": {
						\"bigfile/BIG\": 1000
					}
				},
				{
					\"endpoint\": \"/chunk/{offset}\",
					\"mod_seq\": \"5\",
					\"rate_type\": \"request\",
					\"rates\": {					
						\"bigfile/BIG\": \"100000\"
					}
				}
			]
		}
	}">>,
	{ok, ParsedConfig} = big_config:parse(Config),
	ExpectedConfig = sample_p3_config(big_util:decode(<<?DEPOSIT_ADDRESS>>), -1000000, 2),
	?assertEqual(ExpectedConfig, ParsedConfig#config.p3).

checksum_parse_test() ->
	Config = <<"{
		\"p3\": {
			\"payments\": {
				\"bigfile/BIG\": {
					\"address\": \"gCmvhZDHpVFKQ7spt8XHxe5FGtJSxcIHxcNpBFKt9NI:ToOiTg\",
					\"minimum_balance\": \"-1000000\",
					\"confirmations\": 2
				}
			},
			\"services\": [
				{
					\"endpoint\": \"/price/{bytes}\",
					\"mod_seq\": 1,
					\"rate_type\": \"request\",
					\"rates\": {
						\"bigfile/BIG\": 1000
					}
				},
				{
					\"endpoint\": \"/chunk/{offset}\",
					\"mod_seq\": \"5\",
					\"rate_type\": \"request\",
					\"rates\": {					
						\"bigfile/BIG\": \"100000\"
					}
				}
			]
		}
	}">>,
	{ok, ParsedConfig} = big_config:parse(Config),
	ExpectedConfig = sample_p3_config(big_util:decode(<<?DEPOSIT_ADDRESS>>), -1000000, 2),
	?assertEqual(ExpectedConfig, ParsedConfig#config.p3).

unsupported_payments_asset_parse_error_test() ->
	Config = <<"{
		\"p3\": {
			\"payments\": {
				\"bitcoin/BTC\": {
					\"address\": \"gCmvhZDHpVFKQ7spt8XHxe5FGtJSxcIHxcNpBFKt9NI\",
					\"minimum_balance\": \"-1000000\",
					\"confirmations\": 2
				}
			},
			\"services\": []
		}
	}">>,
	?assertMatch(
		{error, {bad_format, p3, _}, _}, big_config:parse(Config)).

bad_address_parse_error_test() ->
	Config = <<"{
		\"p3\": {
			\"payments\": {
				\"bigfile/BIG\": {
					\"address\": \"gCmvhZDHpVFKQ7spt8XHxe5FGtJSxcIHxcNpBFKt9NI:BAD_CHECKSUM\",
					\"minimum_balance\": \"-1000000\",
					\"confirmations\": 2
				}
			},
			\"services\": []
		}
	}">>,
	?assertMatch(
		{error, {bad_format, p3, _}, _}, big_config:parse(Config)).

bad_minimum_balance_parse_error_test() ->
	Config = <<"{
		\"p3\": {
			\"payments\": {
				\"bigfile/BIG\": {
					\"address\": \"D2z8wfCSpkcP3pw23l6p-Yw6GMuwlZUM0i2dSCpZIrM\",
					\"minimum_balance\": \"5.5\",
					\"confirmations\": 2
				}
			},
			\"services\": []
		}
	}">>,
	?assertMatch(
		{error, {bad_format, p3, _}, _}, big_config:parse(Config)).

bad_confirmations_parse_error_test() ->
	Config = <<"{
		\"p3\": {
			\"payments\": {
				\"bigfile/BIG\": {
					\"address\": \"D2z8wfCSpkcP3pw23l6p-Yw6GMuwlZUM0i2dSCpZIrM\",
					\"minimum_balance\": \"-1000000\",
					\"confirmations\": \"abc\"
				}
			},
			\"services\": []
		}
	}">>,
	?assertMatch(
		{error, {bad_format, p3, _}, _}, big_config:parse(Config)).

bad_payments_token_error_test() ->
	Config = <<"{
		\"p3\": {
			\"payments\": {
				\"bigfile/BIG\": {
					\"address\": \"D2z8wfCSpkcP3pw23l6p-Yw6GMuwlZUM0i2dSCpZIrM\",
					\"minimum_balance\": \"-1000000\",
					\"confirmations\": 2,
					\"invalid\": \"field\"
				}
			},
			\"services\": []
		}
	}">>,
	?assertMatch(
		{error, {bad_format, p3, _}, _}, big_config:parse(Config)).

no_service_list_parse_error_test() ->
	Config = <<"{
		\"p3\": {
			\"services\": {
				\"endpoint\": \"/price/{bytes}\",
				\"mod_seq\": 1,
				\"rate_type\": \"request\",
				\"rates\": {
					\"bigfile/BIG\": \"1000\"
				}
			}
		}
	}">>,
	?assertMatch(
		{error, {bad_format, p3, _}, _}, big_config:parse(Config)).

bad_service_token_parse_error_test() ->
	Config = <<"{
		\"p3\": {
			\"services\": [{
				\"endpoint\": \"/price/{bytes}\",
				\"mod_seq\": 1,
				\"invalid\": \"value\",
				\"rate_type\": \"request\",
				\"rates\": {
					\"bigfile/BIG\": \"1000\"
				}
			}]
		}
	}">>,
	?assertMatch(
		{error, {bad_format, p3, _}, _}, big_config:parse(Config)).

modseq_not_integer_parse_error_test() ->
	Config = <<"{
		\"p3\": {
			\"services\": [{
				\"endpoint\": \"/price/{bytes}\",
				\"mod_seq\": \"a\",
				\"rate_type\": \"request\",
				\"rates\": {
					\"bigfile/BIG\": \"1000\"
				}
			}]
		}
	}">>,
	?assertMatch(
		{error, {bad_format, p3, _}, _}, big_config:parse(Config)).

bad_rates_token_parse_error_test() ->
	Config = <<"{
		\"p3\": {
			\"services\": [{
				\"endpoint\": \"/price/{bytes}\",
				\"mod_seq\": 1,
				\"rate_type\": \"request\",
				\"rates\": {
					\"bigfile/BIG\": \"1000\",
					\"invalid\": \"value\"
				}
			}]
		}
	}">>,
	?assertMatch(
		{error, {bad_format, p3, _}, _}, big_config:parse(Config)).

%% @doc the XXX_validate_test() tests assert that a correctly parsed #p3_config record is
%% correctly validated by the semantic/business logic rules.
no_p3_validate_test() ->
	P3Config = #p3_config{},
	?assertEqual(
		{ok, P3Config},
		big_p3_config:validate_config(#config{})).

empty_p3_validate_test() ->
	P3Config = #p3_config{},
	?assertEqual(
		{ok, P3Config},
		big_p3_config:validate_config(config_fixture(P3Config))).

empty_payments_and_services_validate_test() ->
	P3Config = #p3_config{ payments = #{}, services = #{}},
	?assertEqual(
		{ok, P3Config},
		big_p3_config:validate_config(config_fixture(P3Config))).

basic_validate_test() ->
	P3Config = #p3_config{
		payments = #{
			?BIGFILE_BIG => #p3_payment{
				address = big_util:decode(<<?DEPOSIT_ADDRESS>>),
				minimum_balance = -1000000,
				confirmations = 2	
			}
		},
		services = #{
			<<"/price/{bytes}">> => #p3_service{
				endpoint = <<"/price/{bytes}">>,
				mod_seq = 1,
				rate_type = <<"request">>,
				rates = #{
					?BIGFILE_BIG => 1000
				}
			},
			<<"/chunk/{offset}">> => #p3_service{
				endpoint = <<"/chunk/{offset}">>,
				mod_seq = 5,
				rate_type = <<"request">>,
				rates = #{
					?BIGFILE_BIG => 100000
				}
			}
		}
	},
	?assertEqual(
		{ok, P3Config},
		big_p3_config:validate_config(config_fixture(P3Config))).

empty_payments_validate_test() ->
	P3Config = #p3_config{
			payments = #{},
			services = #{
			<<"/price/{bytes}">> => #p3_service{
				endpoint = <<"/price/{bytes}">>,
				mod_seq = 1,
				rate_type = <<"request">>,
				rates = #{
					?BIGFILE_BIG => 1000
				}
			},
			<<"/chunk/{offset}">> => #p3_service{
				endpoint = <<"/chunk/{offset}">>,
				mod_seq = 5,
				rate_type = <<"request">>,
				rates = #{
					?BIGFILE_BIG => 100000
				}
			}
		}
	},
	?assertEqual(
		{ok, P3Config},
		big_p3_config:validate_config(config_fixture(P3Config))).

empty_services_validate_test() ->
	P3Config = #p3_config{
		payments = #{
			?BIGFILE_BIG => #p3_payment{
				address = big_util:decode(<<?DEPOSIT_ADDRESS>>),
				minimum_balance = -1000000,
				confirmations = 2	
			}
		},
		services = #{}
	},
	?assertEqual(
		{ok, P3Config},
		big_p3_config:validate_config(config_fixture(P3Config))).

bad_payments_asset_validate_test() ->
	P3Config = #p3_config{
		payments = #{
			<<"bitcon/BTC">> => #p3_payment{
				address = big_util:decode(<<?DEPOSIT_ADDRESS>>),
				minimum_balance = -1000000,
				confirmations = 2	
			}
		},
		services = #{}
	},
	?assertMatch(
		{stop, _},
	 	big_p3_config:validate_config(config_fixture(P3Config))).

no_address_validate_test() ->
	P3Config = #p3_config{
		payments = #{
			?BIGFILE_BIG => #p3_payment{
				minimum_balance = -1000000,
				confirmations = 2	
			}
		},
		services = #{}
	},
	?assertMatch(
		{stop, _},
	 	big_p3_config:validate_config(config_fixture(P3Config))).

bad_minimum_balance_validate_test() ->
	P3Config = #p3_config{
		payments = #{
			?BIGFILE_BIG => #p3_payment{
				address = big_util:decode(<<?DEPOSIT_ADDRESS>>),
				minimum_balance = <<"-1000000">>,
				confirmations = 2	
			}
		},
		services = #{}
	},
	?assertMatch(
		{stop, _},
	 	big_p3_config:validate_config(config_fixture(P3Config))).

float_minimum_balance_validate_test() ->
	P3Config = #p3_config{
		payments = #{
			?BIGFILE_BIG => #p3_payment{
				address = big_util:decode(<<?DEPOSIT_ADDRESS>>),
				minimum_balance = -2.5,
				confirmations = 2	
			}
		},
		services = #{}
	},
	?assertMatch(
		{stop, _},
	 	big_p3_config:validate_config(config_fixture(P3Config))).

bad_confirmations_validate_test() ->
	P3Config = #p3_config{
		payments = #{
			?BIGFILE_BIG => #p3_payment{
				address = big_util:decode(<<?DEPOSIT_ADDRESS>>),
				minimum_balance = -1000000,
				confirmations = "2"
			}
		},
		services = #{}
	},
	?assertMatch(
		{stop, _},
	 	big_p3_config:validate_config(config_fixture(P3Config))).

float_confirmations_validate_test() ->
	P3Config = #p3_config{
		payments = #{
			?BIGFILE_BIG => #p3_payment{
				address = big_util:decode(<<?DEPOSIT_ADDRESS>>),
				minimum_balance = -1000000,
				confirmations = 2.5
			}
		},
		services = #{}
	},
	?assertMatch(
		{stop, _},
	 	big_p3_config:validate_config(config_fixture(P3Config))).

all_endpoints_validate_test() ->
	Endpoints = [
		<<"/info">>, <<"/time">>, <<"/tx/pending">>,
		<<"/queue">>, <<"/tx/{hash}/status">>, <<"/tx/{hash}">>, <<"/tx2/{hash}">>,
		<<"/unconfirmed_tx/{hash}">>, <<"/unconfirmed_tx2/{hash}">>, <<"/arql">>,
		<<"/tx/{hash}/data.{ext}">>, <<"/sync_buckets">>, <<"/data_sync_record">>,
		<<"/data_sync_record/{start }/{limit}">>, <<"/chunk/{offset}">>, <<"/chunk2/{offset}">>,
		<<"/tx/{hash}/offset">>, <<"/chunk">>, <<"/block_announcement">>, <<"/block">>,
		<<"/block2">>, <<"/wallet">>, <<"/tx">>, <<"/tx2">>, <<"/unsigned_tx">>, <<"/peers">>,
		<<"/price/{bytes}">>, <<"/price2/{bytes}">>, <<"/optimistic_price/{bytes}">>,
		<<"/price/{bytes}/{address}">>, <<"/price2/{bytes}/{address}">>,
		<<"/optimistic_price/{bytes}/{address}">>, <<"/v2price/{bytes}">>,
		<<"/v2price/{bytes}/{address}">>, <<"/reward_history/{block_hash}">>, <<"/hash_list">>,
		<<"/block_index">>, <<"/block_index2">>, <<"/hash_list/{from}/{to}">>,
		<<"/block_index/{from}/{to}">>, <<"/block_index2/{from}/{to}">>, 
		<<"/recent_hash_list">>, <<"/recent_hash_list_diff">>, <<"/wallet_list">>,
		<<"/wallet_list/{root_hash}">>, <<"/wallet_list/{root_hash}/{cursor}">>,
		<<"/wallet_list/{root_hash}/{addr}/balance">>, <<"/peers">>,
		<<"/wallet/{addr}/balance">>, <<"/wallet/{addr}/last_tx">>, <<"/tx_anchor">>,
		<<"/wallet/{addr}/txs">>, <<"/wallet/{addr}/txs/{earliest_tx}">>,
		<<"/wallet/{addr}/deposits">>, <<"/wallet/{addr}/deposits/{earliest_deposit}">>,
		<<"/block/height/{height}">>, <<"/block/hash/{indep_hash}">>,
		<<"/block2/height/{height}">>, <<"/block2/hash/{indep_hash}">>,
		<<"/block/{type}/{id_bin}/{field}">>,
		<<"/block/height/{height}/wallet/{addr}/balance">>, <<"/block/current">>,
		<<"/tx/{hash}/{field}">>, <<"/height">>, <<"/vdf">>,
		<<"/balance/{address}/{network}/{token}">>, <<"/rates">>
	],
	lists:foreach(
		fun(Endpoint) ->
			P3Config = #p3_config{
				payments = #{},
				services = #{
					Endpoint => #p3_service{
						endpoint = Endpoint,
						mod_seq = 1,
						rate_type = <<"request">>,
						rates = #{
								?BIGFILE_BIG => 100000
						}
					}
				}
			},
			?assertEqual(
				{ok, P3Config},
				big_p3_config:validate_config(config_fixture(P3Config)))
		end,
		Endpoints).

no_endpoint_validate_test() ->
	P3Config = #p3_config{
		payments = #{},
		services = #{
			<<>> => #p3_service{
				mod_seq = 1,
				rate_type = <<"request">>,
				rates = #{
					?BIGFILE_BIG => 100000
				}
			}
		}
	},
	?assertMatch(
		{stop, _},
	 	big_p3_config:validate_config(config_fixture(P3Config))).

bad_endpoint_validate_test() ->
	P3Config = #p3_config{
		payments = #{},
		services = #{
			<<"https://mydomain.com/price/{bytes}">> => #p3_service{
				endpoint = <<"https://mydomain.com/price/{bytes}">>,
				mod_seq = 1,
				rate_type = <<"request">>,
				rates = #{
					?BIGFILE_BIG => 100000
				}
			}
		}
	},
	?assertMatch(
		{stop, _},
	 	big_p3_config:validate_config(config_fixture(P3Config))).

no_mod_seq_validate_test() ->
	P3Config = #p3_config{
		payments = #{},
		services = #{
			<<"/price/{bytes}">> => #p3_service{
				endpoint = <<"/price/{bytes}">>,
				rate_type = <<"request">>,
				rates = #{
					?BIGFILE_BIG => 100000
				}
			}
		}
	},
	?assertMatch(
		{stop, _},
	 	big_p3_config:validate_config(config_fixture(P3Config))).

bad_mod_seq_validate_test() ->
	P3Config = #p3_config{
		payments = #{},
		services = #{
			<<"/price/{bytes}">> => #p3_service{
				endpoint = <<"/price/{bytes}">>,
				mod_seq = "1",
				rate_type = <<"request">>,
				rates = #{
					?BIGFILE_BIG => 100000
				}
			}
		}
	},
	?assertMatch(
		{stop, _},
	 	big_p3_config:validate_config(config_fixture(P3Config))).

no_rates_validate_test() ->
	P3Config = #p3_config{
		payments = #{},
		services = #{
			<<"/price/{bytes}">> => #p3_service{
				endpoint = <<"/price/{bytes}">>,
				mod_seq = 1,
				rate_type = <<"request">>
			}
		}
	},
	?assertMatch(
		{stop, _},
	 	big_p3_config:validate_config(config_fixture(P3Config))).

bad_rates_validate_test() ->
	P3Config = #p3_config{
		payments = #{},
		services = #{
			<<"/price/{bytes}">> => #p3_service{
				endpoint = <<"/price/{bytes}">>,
				mod_seq = 1,
				rate_type = <<"request">>,
				rates = 100000
			}
		}
	},
	?assertMatch(
		{stop, _},
	 	big_p3_config:validate_config(config_fixture(P3Config))).

invalid_rates_asset_validate_test() ->
	P3Config = #p3_config{
		payments = #{},
		services = #{
			<<"/price/{bytes}">> => #p3_service{
				endpoint = <<"/price/{bytes}">>,
				mod_seq = 1,
				rate_type = <<"request">>,
				rates = #{
					<<"bitcoin/BTC">> => 1
				}
			}
		}
	},
	?assertMatch(
		{stop, _},
	 	big_p3_config:validate_config(config_fixture(P3Config))).

bad_rates_price_validate_test() ->
	P3Config = #p3_config{
		payments = #{},
		services = #{
			<<"/price/{bytes}">> => #p3_service{
				endpoint = <<"/price/{bytes}">>,
				mod_seq = 1,
				rate_type = <<"request">>,
				rates = #{
					?BIGFILE_BIG => "abc"
				}
			}
		}
	},
	?assertMatch(
		{stop, _},
	 	big_p3_config:validate_config(config_fixture(P3Config))).

string_rates_price_validate_test() ->
	P3Config = #p3_config{
		payments = #{},
		services = #{
			<<"/price/{bytes}">> => #p3_service{
				endpoint = <<"/price/{bytes}">>,
				mod_seq = 1,
				rate_type = <<"request">>,
				rates = #{
					?BIGFILE_BIG => "1000"
				}
			}
		}
	},
	?assertMatch(
		{stop, _},
	 	big_p3_config:validate_config(config_fixture(P3Config))).

no_rate_type_validate_test() ->
	P3Config = #p3_config{
		payments = #{},
		services = #{
			<<"/price/{bytes}">> => #p3_service{
				endpoint = <<"/price/{bytes}">>,
				mod_seq = 1,
				rates = #{
					?BIGFILE_BIG => 100000
				}
			}
		}
	},
	?assertMatch(
		{stop, _},
	 	big_p3_config:validate_config(config_fixture(P3Config))).

string_rate_type_validate_test() ->
	P3Config = #p3_config{
		payments = #{},
		services = #{
			<<"/price/{bytes}">> => #p3_service{
				endpoint = <<"/price/{bytes}">>,
				mod_seq = 1,
				rate_type = "request",
				rates = #{
					?BIGFILE_BIG => 100000
				}
			}
		}
	},
	?assertMatch(
		{stop, _},
	 	big_p3_config:validate_config(config_fixture(P3Config))).

bad_rate_type_validate_test() ->
	P3Config = #p3_config{
		payments = #{},
		services = #{
			<<"/price/{bytes}">> => #p3_service{
				endpoint = <<"/price/{bytes}">>,
				mod_seq = 1,
				rate_type = <<"invalid">>,
				rates = #{
					?BIGFILE_BIG => 100000
				}
			}
		}
	},
	?assertMatch(
		{stop, _},
	 	big_p3_config:validate_config(config_fixture(P3Config))).

%% ------------------------------------------------------------------
%% @doc the rates_endpoint_test_ tests the /rates endpoint response.
%% ------------------------------------------------------------------
rates_endpoint_test_() ->
	[
		{timeout, 120, fun test_no_rates_endpoint/0},
		{timeout, 120, fun test_empty_rates_endpoint/0},
		{timeout, 120, fun test_empty_payments_and_services_rates_endpoint/0},
		{timeout, 120, fun test_rates_endpoint/0}
	].

test_no_rates_endpoint() ->
	RewardAddress = big_wallet:to_address(big_wallet:new_keyfile()),
	[B0] = big_weave:init(),
	{ok, Config} = application:get_env(bigfile, config),
	big_test_node:start(B0, RewardAddress, Config),

	{<<"200">>, Body} = get_rates(),
	DecodedBody = jiffy:decode(Body, [return_maps]),
	?assertEqual(
		#{<<"endpoints">> => [],<<"payment_methods">> => #{}},
		DecodedBody
	).

test_empty_rates_endpoint() ->
	RewardAddress = big_wallet:to_address(big_wallet:new_keyfile()),
	[B0] = big_weave:init(),
	{ok, BaseConfig} = application:get_env(bigfile, config),
	try
		Config = BaseConfig#config{ p3 = empty_p3_config() },
		big_test_node:start(B0, RewardAddress, Config),

		{<<"200">>, Body} = get_rates(),
		DecodedBody = jiffy:decode(Body, [return_maps]),
		?assertEqual(
			#{<<"endpoints">> => [],<<"payment_methods">> => #{}},
			DecodedBody
		)
	after
		ok = application:set_env(bigfile, config, BaseConfig)
	end.

test_empty_payments_and_services_rates_endpoint() ->
	RewardAddress = big_wallet:to_address(big_wallet:new_keyfile()),
	[B0] = big_weave:init(),
	{ok, BaseConfig} = application:get_env(bigfile, config),
	try
		P3Config = #p3_config{ payments = #{}, services = #{}},
		Config = BaseConfig#config{ p3 = P3Config },
		big_test_node:start(B0, RewardAddress, Config),

		{<<"200">>, Body} = get_rates(),
		DecodedBody = jiffy:decode(Body, [return_maps]),
		?assertEqual(
			#{<<"endpoints">> => [],<<"payment_methods">> => #{}},
			DecodedBody
		)
	after
		ok = application:set_env(bigfile, config, BaseConfig)
	end.

test_rates_endpoint() ->
	{_, Pub1} = big_wallet:new(),
	RewardAddress = big_wallet:to_address(big_wallet:new_keyfile()),
	DepositAddress = big_wallet:to_address(Pub1),
	EncodedDepositAddress = big_util:encode(DepositAddress),
	[B0] = big_weave:init(),
	{ok, BaseConfig} = application:get_env(bigfile, config),
	try
		Config = BaseConfig#config{ p3 = sample_p3_config(DepositAddress, -100, 3) },
		big_test_node:start(B0, RewardAddress, Config),

		{<<"200">>, Body} = get_rates(),
		DecodedBody = jiffy:decode(Body, [return_maps]),
		?assertEqual(
			#{
				<<"payment_methods">> => #{
					<<"bigfile">> => #{
						<<"BIG">> => #{
							<<"minimum_balance">> => -100,
							<<"confirmations">> => 3,
							<<"address">> => EncodedDepositAddress
						}
					}
				},
				<<"endpoints">> => [
					#{
						<<"rates">> => #{
							<<"description">> => <<"Price per request">>,
							<<"bigfile">> => #{
								<<"BIG">> => #{
									<<"price">> => 1000,
									<<"address">> => EncodedDepositAddress
								}
							}
						},
						<<"modSeq">> => 1,
						<<"endpoint">> => <<"/price/{bytes}">>
					},
					#{
						<<"rates">> => #{
							<<"description">> => <<"Price per request">>,
							<<"bigfile">> => #{
								<<"BIG">> => #{
									<<"price">> => 100000,
									<<"address">> => EncodedDepositAddress
								}
							}
						},
						<<"modSeq">> => 5,
						<<"endpoint">> => <<"/chunk/{offset}">>
					}
				]
			},
			DecodedBody
		)
	after
		ok = application:set_env(bigfile, config, BaseConfig)
	end.

%% ------------------------------------------------------------------
%% Helper functions
%% ------------------------------------------------------------------

get_rates() ->
	{ok,{{Status, _}, _, Rates, _, _}} = http_request(
		raw_request(<<"GET">>, <<"/rates">>)
	),
	{Status, Rates}.

config_fixture(P3Config) ->
	#config{ p3 = P3Config }.

sample_p3_config() ->
	sample_p3_config(big_util:decode(<<?DEPOSIT_ADDRESS>>)).
sample_p3_config(Address) ->
	sample_p3_config(Address, 0, 2, 1000).
sample_p3_config(Address, MinimumBalance, Confirmations) ->
	sample_p3_config(Address, MinimumBalance, Confirmations, 1000).
sample_p3_config(Address, MinimumBalance, Confirmations, Rate) when
		is_integer(MinimumBalance),
		is_integer(Confirmations) ->
	#p3_config{
		payments = #{
			?BIGFILE_BIG => #p3_payment{
				address = Address,
				minimum_balance = MinimumBalance,
				confirmations = Confirmations
			}
		},
		services = #{
			<<"/price/{bytes}">> => #p3_service{
				endpoint = <<"/price/{bytes}">>,
				mod_seq = 1,
				rate_type = <<"request">>,
				rates = #{
					?BIGFILE_BIG => Rate
				}
			},
			<<"/chunk/{offset}">> => #p3_service{
				endpoint = <<"/chunk/{offset}">>,
				mod_seq = 5,
				rate_type = <<"request">>,
				rates = #{
					?BIGFILE_BIG => Rate
				}
			}
		}
	}.

empty_p3_config() ->
	#p3_config{}.
