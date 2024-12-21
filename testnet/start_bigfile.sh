#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
CONFIG_FILE="${SCRIPT_DIR}/config/testnet-1.json"

# Dizinleri oluştur
mkdir -p /bigfile-data/{blocks,txs,wallet_lists,data_sync_state}
mkdir -p logs
mkdir -p metrics

# Eski verileri temizle
rm -rf /bigfile-data/blocks/*
rm -rf /bigfile-data/txs/*
rm -rf /bigfile-data/wallet_lists/*
rm -rf /bigfile-data/data_sync_state/*

# Genesis dosyalarını kopyala
cp -r data/genesis_txs/* /bigfile-data/txs/
cp data/genesis_wallets.csv /bigfile-data/wallet_lists/

# Node'u başlat
screen -dmS bigfile ./_build/prod/rel/arweave/bin/arweave start config_file $CONFIG_FILE