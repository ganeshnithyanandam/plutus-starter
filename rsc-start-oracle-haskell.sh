#!/bin/bash

pkh=$1
echo "Start oracle with datum pkh as $1"

cabal run start-oracle $WID -- $1
