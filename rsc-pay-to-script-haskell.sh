#!/bin/bash

amt=$1
echo "Pay amount to script"
cabal run pay-to-script $WID -- $1
