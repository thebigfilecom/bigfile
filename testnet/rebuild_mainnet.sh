#!/bin/bash

ARWEAVE_DIR="$(cd "$(dirname "$0")/.." && pwd)"

if ! $ARWEAVE_DIR/testnet/assert_testnet.sh; then
	echo "Error: This script must be run on a testnet server."
	exit 1
fi

mkdir -p /bigfile-build/mainnet
rm -rf /bigfile-build/mainnet/*

echo "$0 $@" > /bigfile-build/mainnet/build.command

cd $ARWEAVE_DIR
rm -rf $ARWEAVE_DIR/_build/prod/rel/bigfile/*
$ARWEAVE_DIR/rebar3 as prod tar
tar xf $ARWEAVE_DIR/_build/prod/rel/bigfile/bigfile-*.tar.gz -C /bigfile-build/mainnet

