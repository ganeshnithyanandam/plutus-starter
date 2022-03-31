#!/bin/bash

PROJECT_DIR=/home/gannit/cardano/repos/plutus-starter

TESTNET_MAGIC=1097911063
TESTNODE_DIR=/home/gannit/cardano/plutus-apps/plutus-pab/test-node

#name=$1
#passphrase=$2
#file=$3
name=rsc-wallet
passphrase=rsc123456789
seedFileName=rsc-wallet-seed.json
file=$TESTNODE_DIR/testnet/my-wallets/$seedFileName
echo "creating wallet with name $name passphrase $passphrase"

phrase=$(cardano-wallet recovery-phrase generate)

x=''
sep=''
for word in $phrase
do
    x=$x$sep'"'$word'"'
    sep=', '
done

cat > $file <<- EOM
{ "name": "$name"
, "mnemonic_sentence": [$x]
, "passphrase": "$passphrase"
}
EOM
echo "saved restoration file to $file"

WORK_DIR=$PROJECT_DIR/deploy/testnet
cd $WORK_DIR
cp $file $WORK_DIR/$seedFileName
