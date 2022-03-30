#!/usr/bin/env bash

### Start the wallet server
export PLUTUS=/home/gannit/cardano/plutus-apps
export TESTNET_MAGIC=1097911063
export TESTNODE_DIR=$PLUTUS/plutus-pab/test-node
cd $TESTNODE_DIR

cabal exec -- plutus-pab-examples \
  --config testnet/pab-config.yml migrate

cabal exec -- plutus-pab-examples \
  --config testnet/pab-config.yml webserver \
  --passphrase pab123456789

