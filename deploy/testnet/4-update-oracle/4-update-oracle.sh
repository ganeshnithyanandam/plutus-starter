## This is for testnet

PROJECT_DIR=/home/gannit/cardano/repos/plutus-starter
TESTNET_MAGIC=1097911063
TESTNODE_DIR=/home/gannit/cardano/plutus-apps/plutus-pab/test-node
WORK_DIR=$PROJECT_DIR/deploy/testnet
cd $WORK_DIR
export oracleAddress=$(cat $WORK_DIR/oracle.addr)
echo $oracleAddress
cardano-cli query utxo --address $oracleAddress --testnet-magic $TESTNET_MAGIC

    