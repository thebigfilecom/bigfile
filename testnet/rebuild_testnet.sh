#!/bin/bash

BIGFILE_DIR="$(cd "$(dirname "$0")/.." && pwd)"

if ! $BIGFILE_DIR/testnet/assert_testnet.sh; then
	echo "Error: This script must be run on a testnet server."
	exit 1
fi

mkdir -p /bigfile-build/testnet
rm -rf /bigfile-build/testnet/*

echo "$0 $@" > /bigfile-build/testnet/build.command

cd $BIGFILE_DIR
rm -rf $BIGFILE_DIR/_build/testnet/rel/bigfile/*
$BIGFILE_DIR/rebar3 as testnet tar
tar xf $BIGFILE_DIR/_build/testnet/rel/bigfile/bigfile-*.tar.gz -C /bigfile-build/testnet
