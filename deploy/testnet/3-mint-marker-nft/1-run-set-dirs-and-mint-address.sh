#!/usr/bin/env bash

## This doc is for testnet

### Directories for this minting
PROJECT_DIR=/home/gannit/cardano/repos/plutus-starter
export TESTNET_MAGIC=1097911063
export TESTNODE_DIR=/home/gannit/cardano/plutus-apps/plutus-pab/test-node
cd $TESTNODE_DIR

export WORK_DIR=$PROJECT_DIR/deploy/testnet
cd $WORK_DIR

####Checklist ref : 2 c
### Generate keys and address of ADR to be minted
cardano-cli address key-gen --verification-key-file ver-ctl.vkey --signing-key-file sig-ctl.skey
cardano-cli address build --payment-verification-key-file ver-ctl.vkey --out-file control-wallet-address.addr --testnet-magic $TESTNET_MAGIC

export controlWalletAddress=$(cat control-wallet-address.addr)
echo $controlWalletAddress
cardano-cli query utxo --address $controlWalletAddress --testnet-magic $TESTNET_MAGIC
