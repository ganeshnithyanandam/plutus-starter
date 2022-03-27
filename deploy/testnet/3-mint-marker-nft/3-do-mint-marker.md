## This doc is for testnet 

### Set directories:
PROJECT_DIR=/home/gannit/cardano/repos/plutus-starter
TESTNET_MAGIC=1097911063
CARDANO_NODE_SOCKET_PATH=$TESTNODE_DIR/testnet/node.sock
export TESTNODE_DIR=/home/gannit/cardano/plutus-apps/plutus-pab/test-node
export WORK_DIR=$PROJECT_DIR/deploy/testnet
cd $WORK_DIR

rm -r policy
mkdir policy
POLICY_DIR=$WORK_DIR/policy

#### Set variables
export controlWalletAddress=$(cat $WORK_DIR/control-wallet-address.addr)
echo $controlWalletAddress

hrtokenname="TADROrcl"
tokenname=$(echo -n $hrtokenname | xxd -b -ps -c 80 | tr -d '\n')
tokenamount=1


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
mintUnsignedFile=$WORK_DIR/mint-tx.unsigned
mintSignedFile=$WORK_DIR/mint-tx.signed
policyFile=$WORK_DIR/mintmarker.plutus
policyid=$(cardano-cli transaction policyid --script-file $policyFile)
v="$tokenamount $policyid.$tokenname"
skeyFile=$WORK_DIR/sig-ctl.skey



cardano-cli query utxo --address $controlWalletAddress --testnet-magic $TESTNET_MAGIC

```
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
40ed073aac0525db8603d1fbcbcce29e8ddc41b8ec6ae99aef695ad5c87ea171     0        30000000 lovelace + TxOutDatumNone
```

txhash="40ed073aac0525db8603d1fbcbcce29e8ddc41b8ec6ae99aef695ad5c87ea171"
txix="0"
funds="30000000"
fee="0"
output="0"

#### Check variables 
echo $fee
echo $controlWalletAddress
echo $output
echo $tokenamount
echo $policyid
echo $tokenname

#### Build mint transaction
cardano-cli transaction build \
--alonzo-era \
--testnet-magic $TESTNET_MAGIC \
--tx-in $txhash#$txix \
--tx-in-collateral $txhash#$txix \
--tx-out "$controlWalletAddress + 1500000 lovelace + $v" \
--mint "$v" \
--mint-script-file $policyFile \
--mint-redeemer-file $WORK_DIR/unit.json \
--change-address $controlWalletAddress \
--protocol-params-file $WORK_DIR/protocol.json \
--out-file $mintUnsignedFile 

cardano-cli transaction sign \
--tx-body-file $mintUnsignedFile \
--signing-key-file $skeyFile \
--testnet-magic $TESTNET_MAGIC \
--out-file $mintSignedFile

cardano-cli transaction submit \
--testnet-magic $TESTNET_MAGIC \
--tx-file $mintSignedFile


----------------------------------------------------------------------
##### Minting is complete
----------------------------------------------------------------------

####Check minting address:

cardano-cli query utxo --address $controlWalletAddress --testnet-magic $TESTNET_MAGIC

```
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f695bd3708f19747503e567c8e5e954b0e7dc96d7a474f7ed53078a28112c37f     0        998465941 lovelace + TxOutDatumNone
f695bd3708f19747503e567c8e5e954b0e7dc96d7a474f7ed53078a28112c37f     1        1344798 lovelace + 1000000000 5c59f115778db31d8e83d900a7c5cc4e80c1bfce8ccac2104ab281b3.7465737439414452 + TxOutDatumNone

```