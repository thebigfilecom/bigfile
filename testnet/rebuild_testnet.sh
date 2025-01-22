#!/bin/bash

ARWEAVE_DIR="$(cd "$(dirname "$0")/.." && pwd)"

if ! $ARWEAVE_DIR/testnet/assert_testnet.sh; then
	echo "Error: This script must be run on a testnet server."
	exit 1
fi

mkdir -p /bigfile-build/testnet
rm -rf /bigfile-build/testnet/*

echo "$0 $@" > /bigfile-build/testnet/build.command

cd $ARWEAVE_DIR
rm -rf $ARWEAVE_DIR/_build/testnet/rel/bigfile/*
$ARWEAVE_DIR/rebar3 as testnet tar
tar xf $ARWEAVE_DIR/_build/testnet/rel/bigfile/bigfile-*.tar.gz -C /bigfile-build/testnet
