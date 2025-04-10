#!/bin/bash

BIGFILE_DIR="$(cd "$(dirname "$0")/.." && pwd)"

if ! $BIGFILE_DIR/testnet/assert_testnet.sh; then
	echo "Error: This script must be run on a testnet server."
	exit 1
fi

read -p "Do you really want to delete all files and directories in /bigfile-data \
except for storage_modules and wallets? [y/N] " response

if [[ "$response" =~ ^([yY][eE][sS]|[yY])$ ]]; then
    for item in /bigfile-data/*; do
		filename=$(basename "$item")
        if [[ "$filename" != "wallets" && ! "$filename" =~ ^storage_module ]]; then
            echo rm -rf "$item"
			rm -rf "$item"
        fi
    done
    echo "Cleanup complete!"
else
    echo "Operation cancelled."
fi