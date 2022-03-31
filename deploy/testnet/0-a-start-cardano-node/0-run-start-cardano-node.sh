#!/usr/bin/env bash

### Start the testnet node locally:
export TESTNET_MAGIC=1097911063
export TESTNODE_DIR=/home/gannit/cardano/plutus-apps/plutus-pab/test-node
cd $TESTNODE_DIR
./start-testnet-node.sh
