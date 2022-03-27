## This doc is for testnet 

### Set directories:
PROJECT_DIR=/home/gannit/cardano/repos/plutus-starter
TESTNET_MAGIC=1097911063
export TESTNODE_DIR=/home/gannit/cardano/plutus-apps/plutus-pab/test-node
CARDANO_NODE_SOCKET_PATH=$TESTNODE_DIR/testnet/node.sock
export WORK_DIR=$PROJECT_DIR/deploy/testnet
cd $WORK_DIR


#### Set variables
controlWalletAddress=$(cat $WORK_DIR/control-wallet-address.addr)
oracleAddress=$(cat $WORK_DIR/oracle.addr)
echo $controlWalletAddress
echo $oracleAddress

hrtokenname="TADROrcl"
tokenname=$(echo -n $hrtokenname | xxd -b -ps -c 80 | tr -d '\n')
tokenamount=1

policyFile=$WORK_DIR/mintmarker.plutus
policyid=$(cardano-cli transaction policyid --script-file $policyFile)
### Build the transactions

```
Each transaction in Cardano requires the payment of a fee which — as of now — will mostly be determined by the size of what we want to transmit. The more bytes get sent, the higher the fee.
That's why making a transaction in Cardano is a three-way process.

First, we will build a transaction, resulting in a file. This will be the foundation of how the transaction fee will be calculated.
We use this raw file and our protocol parameters to calculate our fees
Then we need to re-build the transaction, including the correct fee and the adjusted amount we're able to send. Since we send it to ourselves, the output needs to be the amount of our fundings minus the calculated fee.
```


#### Set variables
cardano-cli query protocol-parameters --testnet-magic $TESTNET_MAGIC --out-file $WORK_DIR/protocol.json
setOracleUnsignedFile=$WORK_DIR/set-oracle.unsigned
setOracleSignedFile=$WORK_DIR/set-oracle.signed
v="$tokenamount $policyid.$tokenname"
skeyFile=$WORK_DIR/sig-ctl.skey


cardano-cli query utxo --address $controlWalletAddress --testnet-magic $TESTNET_MAGIC

```
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
65e67d651fe33e0a932ada2e5619139d0e09c7e5357d9ccb4600c06bf380d634     0        28186442 lovelace + TxOutDatumNone
65e67d651fe33e0a932ada2e5619139d0e09c7e5357d9ccb4600c06bf380d634     1        1500000 lovelace + 1 f1f955947c184cbf1bd6de21eb004a9af5b43949d5aeb952467f3c23.544144524f72636c + TxOutDatumNone

```

txhash="65e67d651fe33e0a932ada2e5619139d0e09c7e5357d9ccb4600c06bf380d634"
txix0="0"
txix1="1"
fee="0"
output="0"
oracleLovelaceOut="1689618"

#### Check variables 
echo $fee
echo $controlWalletAddress
echo $output
echo $tokenamount
echo $tokenname

#### Build mint transaction
cardano-cli transaction build \
--alonzo-era \
--testnet-magic $TESTNET_MAGIC \
--tx-in $txhash#$txix0 \
--tx-in $txhash#$txix1 \
--tx-in-collateral $txhash#$txix0 \
--tx-out $oracleAddress+$oracleLovelaceOut+"$tokenamount $policyid.$tokenname"  \
--tx-out-datum-hash-file datum-unused-pkh.json \
--change-address $controlWalletAddress \
--protocol-params-file $WORK_DIR/protocol.json \
--out-file $setOracleUnsignedFile 

cardano-cli transaction sign \
--tx-body-file $setOracleUnsignedFile \
--signing-key-file $skeyFile \
--testnet-magic $TESTNET_MAGIC \
--out-file $setOracleSignedFile

cardano-cli transaction submit \
--testnet-magic $TESTNET_MAGIC \
--tx-file $setOracleSignedFile


----------------------------------------------------------------------
##### Oracle is set
----------------------------------------------------------------------

####Check oracle address:

cardano-cli query utxo --address $controlWalletAddress --testnet-magic $TESTNET_MAGIC
cardano-cli query utxo --address $oracleAddress --testnet-magic $TESTNET_MAGIC

```
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f695bd3708f19747503e567c8e5e954b0e7dc96d7a474f7ed53078a28112c37f     0        998465941 lovelace + TxOutDatumNone
f695bd3708f19747503e567c8e5e954b0e7dc96d7a474f7ed53078a28112c37f     1        1344798 lovelace + 1000000000 5c59f115778db31d8e83d900a7c5cc4e80c1bfce8ccac2104ab281b3.7465737439414452 + TxOutDatumNone

```