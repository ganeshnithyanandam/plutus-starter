#!/bin/bash

pkh=$1
echo "Start oracle with datum pkh as $1"

cabal run update-oracle $WID -- $1
