#!/bin/bash

ARWEAVE_DIR="$(cd "$(dirname "$0")/.." && pwd)"

ALL_NODES+=(
bigfile-testnet-1
)

# Get the current hostname
current_host=$(hostname -f)

# Check if current hostname is in the list of testnet servers
is_testnet_server=0
for server in "${ALL_NODES[@]}"; do
    if [[ "$current_host" == "$server" ]]; then
        is_testnet_server=1
        break
    fi
done

# If not a testnet server, abort
if [[ "$is_testnet_server" -eq 0 ]]; then
    exit 1
fi

mkdir -p /bigfile-build/mainnet
mkdir -p /bigfile-build/testnet
