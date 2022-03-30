#!/usr/bin/env bash

### Start the testnet node locally:
export PLUTUS=/home/gannit/cardano/plutus-apps
export TESTNET_MAGIC=1097911063
export TESTNODE_DIR=$PLUTUS/plutus-pab/test-node
cd $TESTNODE_DIR
./start-testnet-node.sh
