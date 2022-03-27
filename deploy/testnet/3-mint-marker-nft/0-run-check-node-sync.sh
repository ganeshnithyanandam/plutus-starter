#!/usr/bin/env bash

### Start the testnet node locally:
export TESTNET_MAGIC=1097911063
export TESTNODE_DIR=/home/gannit/cardano/plutus-apps/plutus-pab/test-node
export CARDANO_NODE_SOCKET_PATH=$TESTNODE_DIR/testnet/node.sock
cardano-cli query tip --testnet-magic $TESTNET_MAGIC
