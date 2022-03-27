PROJECT_DIR=/home/gannit/cardano/repos/plutus-starter

TESTNET_MAGIC=1097911063
TESTNODE_DIR=/home/gannit/cardano/plutus-apps/plutus-pab/test-node
WORK_DIR=$PROJECT_DIR/deploy/testnet
cd $WORK_DIR

### 1. Create script address
cardano-cli query tip --testnet-magic $TESTNET_MAGIC

cardano-cli address build-script --script-file oracle.plutus --testnet-magic 1097911063 --out-file oracle.addr