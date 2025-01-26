#!/bin/bash

ARWEAVE_DIR="$(cd "$(dirname "$0")/.." && pwd)"

if ! $ARWEAVE_DIR/testnet/assert_testnet.sh; then
	echo "Error: This script must be run on a testnet server."
	exit 1
fi

if [[ ! -f "/bigfile-build/mainnet/bin/start" ]]; then
    echo "BigFile start script not found. Please run rebuild_mainnet.sh first."
	exit 1
fi

config_file="$ARWEAVE_DIR/testnet/config/$(hostname -f).json"
SCREEN_CMD="screen -dmsL arweave /bigfile-build/mainnet/bin/start config_file $config_file vdf_server_trusted_peer vdf-server-3.arweave.xyz"

echo "$SCREEN_CMD"
echo "$SCREEN_CMD" > /bigfile-build/mainnet/run.sh
chmod +x /bigfile-build/mainnet/run.sh

cd /bigfile-build/mainnet
./run.sh
