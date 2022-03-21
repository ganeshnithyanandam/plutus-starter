{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Oracle.Trace where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras                   as Extras
import           Plutus.Contract            as Contract
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (IO, String, show)
import           Wallet.Emulator.Wallet
import           Test.Tasty
import qualified Test.Tasty.HUnit                             as HUnit
import           Data.Monoid                                  (Last (..))
import           Plutus.Contract.Trace      as X

import           Oracle.OffChain

testContract :: IO ()
testContract = runEmulatorTraceIO oracleTrace

alice, bob :: Wallet
alice = X.knownWallet 1
bob = X.knownWallet 2

oracleTrace :: EmulatorTrace ()
oracleTrace = do
    h <- activateContractWallet alice startEndpoint
    {-ownPK <- Contract.ownPaymentPubKeyHash-}
    let pkh = mockWalletPaymentPubKeyHash bob
    callEndpoint @"start" h pkh

    void $ Emulator.waitNSlots 2
    callEndpoint @"inspect" h ()