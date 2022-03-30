#!/usr/bin/env bash

### Start the wallet server
export PLUTUS=/home/gannit/cardano/plutus-apps
export TESTNET_MAGIC=1097911063
export TESTNODE_DIR=$PLUTUS/plutus-pab/test-node
cd $TESTNODE_DIR

cabal exec -- plutus-chain-index --config testnet/chain-index-config.json start-index

