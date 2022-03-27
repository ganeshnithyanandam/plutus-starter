####Checklist ref : 2 d
### Fund the address
For the testnet, you can request funds through the [testnet faucet](https://developers.cardano.org/docs/integrate-cardano/testnet-faucet).
For the mainnet, send some ADA to the address.

### Check balance

PROJECT_DIR=/home/gannit/cardano/repos/plutus-starter
TESTNET_MAGIC=1097911063
TESTNODE_DIR=/home/gannit/cardano/plutus-apps/plutus-pab/test-node
CARDANO_NODE_SOCKET_PATH=$TESTNODE_DIR/testnet/node.sock
cd $TESTNODE_DIR

WORK_DIR=$PROJECT_DIR/deploy/testnet

controlWalletAddress=$(cat $WORK_DIR/control-wallet-address.addr)
echo $controlWalletAddress

cardano-cli query utxo --address $controlWalletAddress --testnet-magic $TESTNET_MAGIC
