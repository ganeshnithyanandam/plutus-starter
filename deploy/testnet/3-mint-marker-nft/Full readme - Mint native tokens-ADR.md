## This doc is for testnet 

####Checklist ref : 2 b
### Start the testnet node locally:
export PLUTUS=/home/gannit/cardano/plutus-apps
export TESTNET_MAGIC=1097911063
export TESTNODE_DIR=/home/gannit/cardano/plutus-apps/plutus-pab/test-node
cd $TESTNODE_DIR
./start-testnet-node.sh

### Query the tip
export CARDANO_NODE_SOCKET_PATH=$TESTNODE_DIR/testnet/node.sock
cardano-cli query tip --testnet-magic $TESTNET_MAGIC

### Directories for this minting
export TESTNET_MAGIC=1097911063
export TESTNODE_DIR=/home/gannit/cardano/plutus-apps/plutus-pab/test-node
cd $TESTNODE_DIR

export WORK_DIR=$TESTNODE_DIR/tokens/rst
cd $WORK_DIR

mkdir policy
export POLICY_DIR=$WORK_DIR/policy

####Checklist ref : 2 c
### Generate keys and address of ADR to be minted
cardano-cli address key-gen --verification-key-file ver-ctl.vkey --signing-key-file sig-ctl.skey
cardano-cli address build --payment-verification-key-file ver-ctl.vkey --out-file control-wallet-address.addr --testnet-magic $TESTNET_MAGIC

export controlWalletAddress=$(cat control-wallet-address.addr)
echo $controlWalletAddress
cardano-cli query utxo --address $controlWalletAddress --testnet-magic $TESTNET_MAGIC

####Checklist ref : 2 d
### Fund the address
For the testnet, you can request funds through the [testnet faucet](https://developers.cardano.org/docs/integrate-cardano/testnet-faucet).
For the mainnet, send some ADA to the address.

####Checklist ref : 2 e
### Export protocol parameters
Protocol parameters are required for transaction calculations. The parameters can be saved in a file called protocol.json with this command:

cardano-cli query protocol-parameters --testnet-magic $TESTNET_MAGIC --out-file protocol.json

####Checklist ref : 2 f
### Policy script
cardano-cli address key-gen \
--verification-key-file $POLICY_DIR/policy-rst.vkey \
--signing-key-file $POLICY_DIR/policy-rst.skey

#### Time lock params for the policy
export slotMintValidUntil=$(expr $(cardano-cli query tip --testnet-magic $TESTNET_MAGIC | jq .slot?) + 3600)
echo $slotMintValidUntil

#### Create policy script 
```
rm $POLICY_DIR/policy-script-rst.script
```

touch $POLICY_DIR/policy-script-rst.script && echo "" > $POLICY_DIR/policy-script-rst.script

echo "{" >> $POLICY_DIR/policy-script-rst.script
echo "  \"type\": \"all\"," >> $POLICY_DIR/policy-script-rst.script
echo "  \"scripts\":" >> $POLICY_DIR/policy-script-rst.script
echo "  [" >> $POLICY_DIR/policy-script-rst.script
echo "   {" >> $POLICY_DIR/policy-script-rst.script
echo "     \"type\": \"before\"," >> $POLICY_DIR/policy-script-rst.script
echo "     \"slot\": $slotMintValidUntil" >> $POLICY_DIR/policy-script-rst.script
echo "   }," >> $POLICY_DIR/policy-script-rst.script
echo "   {" >> $POLICY_DIR/policy-script-rst.script
echo "     \"type\": \"sig\"," >> $POLICY_DIR/policy-script-rst.script
echo "     \"keyHash\": \"$(cardano-cli address key-hash --payment-verification-key-file $POLICY_DIR/policy-rst.vkey)\"" >> $POLICY_DIR/policy-script-rst.script
echo "   }" >> $POLICY_DIR/policy-script-rst.script
echo "  ]" >> $POLICY_DIR/policy-script-rst.script
echo "}" >> $POLICY_DIR/policy-script-rst.script

cat $POLICY_DIR/policy-script-rst.script

### Generate policy ID
To mint the native assets, we need to generate the policy ID from the script file we created.

rm $POLICY_DIR/policyID-rst
cardano-cli transaction policyid --script-file $POLICY_DIR/policy-script-rst.script >> $POLICY_DIR/policyID-rst

####Checklist ref : 2 g
### Set some token properties in required format
hrtokenname="test2ADR"
tokenname=$(echo -n $hrtokenname | xxd -b -ps -c 80 | tr -d '\n')
tokenamount="1000000000"

####Checklist ref : 2 h
### Create metadata
export metadataFile=$WORK_DIR/metadata-rst.json
export ipfsImageHash=QmRzD834bi3FCvTjuycBvWaU72MaHLQjAAgx6DKad14sDN
export thumbnailMimeType=image/jpeg
rm $metadataFile

echo "{" >> $metadataFile       
echo "  \"0\": {" >> $metadataFile
echo "    \"$(cat $POLICY_DIR/policyID-rst)\": {" >> $metadataFile
echo "      \"$(echo $hrtokenname)\": {" >> $metadataFile
echo "        \"id\": \"Loxe-RST-ADR\"," >> $metadataFile
echo "        \"name\": \"ADR token\"," >> $metadataFile
echo "        \"description\": \"Revenue share token issued by Loxe Inc.\"," >> $metadataFile
echo "        \"image\": \"ipfs://$(echo $ipfsImageHash)\"," >> $metadataFile
echo "        \"mediaType\": \"$thumbnailMimeType\"" >> $metadataFile
echo "      }" >> $metadataFile
echo "    }," >> $metadataFile
echo "   \"version\": \"1.0\"" >> $metadataFile
echo "  }" >> $metadataFile
echo "}" >> $metadataFile

cat $metadataFile

####Checklist ref : 2 i   
### Build the transactions
Each transaction in Cardano requires the payment of a fee which — as of now — will mostly be determined by the size of what we want to transmit. The more bytes get sent, the higher the fee.
That's why making a transaction in Cardano is a three-way process.

First, we will build a transaction, resulting in a file. This will be the foundation of how the transaction fee will be calculated.
We use this raw file and our protocol parameters to calculate our fees
Then we need to re-build the transaction, including the correct fee and the adjusted amount we're able to send. Since we send it to ourselves, the output needs to be the amount of our fundings minus the calculated fee.

cardano-cli query utxo --address $controlWalletAddress --testnet-magic $TESTNET_MAGIC

```
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
7d02b18697352ed640cb9ebfbc7e66b08cad5a85b4cc1b10391943f887928dd1     0        200000000 lovelace + TxOutDatumNone

```

#### Set variables
export txhash="7d02b18697352ed640cb9ebfbc7e66b08cad5a85b4cc1b10391943f887928dd1"
export txix="0"
export funds="200000000"
export policyid=$(cat $POLICY_DIR/policyID-rst)
export fee="0"
export output="0"
export mintingscript=$POLICY_DIR/policy-script-rst.script

#### Check variables 
echo $fee
echo $controlWalletAddress
echo $output
echo $tokenamount
echo $policyid
echo $tokenname
echo $slotMintValidUntil
echo $mintingscript

#### Build mint transaction

cardano-cli transaction build \
--alonzo-era \
--testnet-magic $TESTNET_MAGIC \
--witness-override 2 \
--tx-in $txhash#$txix \
--tx-out $controlWalletAddress+$output+"$tokenamount $policyid.$tokenname" \
--change-address $controlWalletAddress \
--mint="$tokenamount $policyid.$tokenname" \
--minting-script-file $mintingscript \
--invalid-hereafter $slotMintValidUntil \
--metadata-json-file $metadataFile  \
--out-file rst-mint-tx.raw

```
set $output based on the result of above command and rerun
```
export output="1344798"

##### Rerun the above `cardano-cli transaction build` command



#### Sign the mint tx
cardano-cli transaction sign  \
--signing-key-file $WORK_DIR/sig-ctl.skey  \
--signing-key-file $POLICY_DIR/policy-rst.skey  \
--testnet-magic $TESTNET_MAGIC \
--tx-body-file $WORK_DIR/rst-mint-tx.raw  \
--out-file rst-mint-tx.signed

#### Submit the tx
cardano-cli transaction submit --tx-file $WORK_DIR/rst-mint-tx.signed --testnet-magic $TESTNET_MAGIC


----------------------------------------------------------------------
##### Minting is complete
----------------------------------------------------------------------

####Checklist ref : 2 j
####Check minting address:

cardano-cli query utxo --address $controlWalletAddress --testnet-magic $TESTNET_MAGIC

```
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
7e777031b4cbfd5ef01dba34541536499857ef297e85c6759520983458b4af0d     0        198465985 lovelace + TxOutDatumNone
7e777031b4cbfd5ef01dba34541536499857ef297e85c6759520983458b4af0d     1        1344798 lovelace + 1000000000 2ad7fd9b0913531eb14a58218304c7b613db71bbe098cc94bb0328d5.7465737432414452 + TxOutDatumNone

```

####Checklist ref : 3 a
### Send token to storage wallet

export storageWalletAddr=$(cat $TESTNODE_DIR/rst-storage-wallet/rst-storage-addr.txt)

##### Build send transaction
export txHash0="7e777031b4cbfd5ef01dba34541536499857ef297e85c6759520983458b4af0d"
export txIx0="0"
export mintTxHash="7e777031b4cbfd5ef01dba34541536499857ef297e85c6759520983458b4af0d"
export mintTxIx="1"
export sendFee="0"
export storageWalletOutput="100000000"


cardano-cli transaction build \
--alonzo-era \
--testnet-magic $TESTNET_MAGIC \
--tx-in $txHash0#$txIx0 \
--tx-in $mintTxHash#$mintTxIx  \
--tx-out $storageWalletAddr+$storageWalletOutput+"$tokenamount $policyid.$tokenname"  \
--change-address $controlWalletAddress \
--out-file rst-send-tx.raw

#### Sign the tx
cardano-cli transaction sign  \
--signing-key-file sig-ctl.skey  \
--testnet-magic $TESTNET_MAGIC \
--tx-body-file rst-send-tx.raw  \
--out-file rst-send-tx.signed

#### Submit the tx
cardano-cli transaction submit --tx-file rst-send-tx.signed --testnet-magic $TESTNET_MAGIC

####Checklist ref : 3 b
####Check that ADR tokens have moved from mint address to storage wallet
cardano-cli query utxo --address $controlWalletAddress --testnet-magic $TESTNET_MAGIC

cardano-cli query utxo --address $storageWalletAddr --testnet-magic $TESTNET_MAGIC


#### Backup the files in $WORK_DIR securely