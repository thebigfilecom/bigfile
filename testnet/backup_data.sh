#!/bin/bash

BIGFILE_DIR="$(cd "$(dirname "$0")/.." && pwd)"

if ! $BIGFILE_DIR/testnet/assert_testnet.sh; then
	echo "Error: This script must be run on a testnet server."
	exit 1
fi

if [ $# -ne 1 ]; then
	echo "backup_data.sh <backup name>"
    exit 1
fi

NAME=$1
BACKUP_DIR="/bigfile-backups/${NAME}/"

if [ -d "$BACKUP_DIR" ]; then
    echo "Error: Backup directory $BACKUP_DIR already exists."
    exit 1
fi

set -x
mkdir -p $BACKUP_DIR
cp -rf /bigfile-data/data_sync_state $BACKUP_DIR
cp -rf /bigfile-data/header_sync_state $BACKUP_DIR
cp -rf /bigfile-data/ar_tx_blacklist $BACKUP_DIR
cp -rf /bigfile-data/disk_cache $BACKUP_DIR
cp -rf /bigfile-data/rocksdb $BACKUP_DIR
cp -rf /bigfile-data/txs $BACKUP_DIR
cp -rf /bigfile-data/wallet_lists $BACKUP_DIR
cp -rf /bigfile-data/wallets $BACKUP_DIR
{ set +x; } 2>/dev/null
echo

