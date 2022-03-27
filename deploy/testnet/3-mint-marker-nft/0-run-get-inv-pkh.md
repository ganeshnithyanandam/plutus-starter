## This doc is for testnet 

### Set directories:
PROJECT_DIR=/home/gannit/cardano/repos/plutus-starter
TESTNET_MAGIC=1097911063
CARDANO_NODE_SOCKET_PATH=$TESTNODE_DIR/testnet/node.sock
export TESTNODE_DIR=/home/gannit/cardano/plutus-apps/plutus-pab/test-node
WORK_DIR=$PROJECT_DIR/deploy/testnet
cd $WORK_DIR

cardano-cli address key-hash --payment-verification-key-file rst-sw-key.vkey --out-file pkhash.pkh
