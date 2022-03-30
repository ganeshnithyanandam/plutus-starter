#!/usr/bin/env bash

### Start the wallet server
export PLUTUS=/home/gannit/cardano/plutus-apps
export TESTNET_MAGIC=1097911063
export TESTNODE_DIR=$PLUTUS/plutus-pab/test-node
cd $TESTNODE_DIR

curl -H "content-type: application/json" -XPOST \
  -d @testnet/my-wallets/restore-wallet-adatar-testnet.json \
  localhost:8090/v2/wallets


#{"balance":{"total":{"quantity":0,"unit":"lovelace"},"available":{"quantity":0,"unit":"lovelace"},"reward":{"quantity":0,"unit":"lovelace"}},"name":"Adatar testnet wallet","id":"501e807bee3322b2ec85289250d1c6abfdc7d467","tip":{"height":{"quantity":0,"unit":"block"},"epoch_number":0,"time":"2019-07-24T20:20:16Z","absolute_slot_number":0,"slot_number":0},"passphrase":{"last_updated_at":"2022-03-29T13:01:46.794518204Z"},"address_pool_gap":20,"state":{"status":"syncing","progress":{"quantity":0,"unit":"percent"}},"delegation":{"next":[],"active":{"status":"not_delegating"}},"assets":{"total":[],"available":[]}}
