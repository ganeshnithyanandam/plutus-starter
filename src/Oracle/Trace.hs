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

alice, bob, charlie :: Wallet
alice = X.knownWallet 1
bob = X.knownWallet 2
charlie = X.knownWallet 3

oracleTrace :: EmulatorTrace ()
oracleTrace = do
    h1 <- activateContractWallet alice useOrclEndpoints
    h2 <- activateContractWallet alice walletEndpoint
    {-h2 <- activateContractWallet alice inspectEndpoint-}
    {-ownPK <- Contract.ownPaymentPubKeyHash-}
    let pkhBob = mockWalletPaymentPubKeyHash bob
    callEndpoint @"start" h1 pkhBob
    let pkhCharlie = mockWalletPaymentPubKeyHash charlie
    callEndpoint @"update" h1 pkhCharlie

    void $ Emulator.waitNSlots 2
    callEndpoint @"payRewards" h2 ()