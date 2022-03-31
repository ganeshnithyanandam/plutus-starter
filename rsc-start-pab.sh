#!/bin/bash

cabal run -- oracle-pab \
  --config testnet/pab-config.yml webserver \
  --passphrase rsc123456789
