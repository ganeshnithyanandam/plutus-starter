{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module PayToWalletTest where

import           PayToWallet
import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import           Ledger
import           Ledger.TimeSlot
import           Plutus.Contract.Test       ((.&&.), walletFundsChange, checkPredicate)
import           Plutus.Contract.Trace      as X
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude
import           Prelude                    (IO, Show (..))
import           Test.Tasty


test1 :: IO ()
test1 = runEmulatorTraceIO $ runTrace1

test2 :: IO ()
test2 = runEmulatorTraceIO $ runTrace2

test3 :: IO ()
test3 = runEmulatorTraceIO $ runTrace3

test4 :: IO ()
test4 = runEmulatorTraceIO $ runTrace4

test5 :: IO ()
test5 = runEmulatorTraceIO $ runTrace5

test6 :: IO ()
test6 = runEmulatorTraceIO $ runTrace6

test7 :: IO ()
test7 = runEmulatorTraceIO $ runTrace7

alice, bob :: Wallet
alice = X.knownWallet 1
bob = X.knownWallet 2


getTT :: ContractHandle (Last ThreadToken) StartAppliedEscrowSchema Text -> EmulatorTrace ThreadToken
getTT h = do
    void $ Emulator.waitNSlots 1
    Last m <- observableState h
    case m of
        Nothing -> getTT h
        Just tt -> Extras.logInfo ("read thread token " ++ show tt) >> return tt

testContractAmount :: Integer
testContractAmount = 80_000_000

testTrancheCount :: Integer
testTrancheCount = 4

testContractStartTime :: POSIXTime
testContractStartTime = slotToBeginPOSIXTime def 10

testContractEndTime :: POSIXTime
testContractEndTime = slotToBeginPOSIXTime def 26

buildPublishParam :: PublishParam
buildPublishParam =
  PublishParam
    { p      = pkh1
    , c      = pkh2
    , st     = startT
    , et     = endT
    , tc     = testTrancheCount
    , ll     = amount
    }
    where
      pkh1      = (pubKeyHash . walletPubKey) $ alice
      pkh2      = (pubKeyHash . walletPubKey) $ bob
      amount    = testContractAmount
      startT    = testContractStartTime
      endT      = testContractEndTime

buildUseParam :: ThreadToken -> UseParam
buildUseParam tt =
  UseParam
    { up      = pkh1
    , uc      = pkh2
    , ust     = startT
    , uet     = endT
    , ull     = amount
    , utc     = testTrancheCount
    , uttn    = tt
    }
    where
      pkh1      = (pubKeyHash . walletPubKey) $ alice
      pkh2      = (pubKeyHash . walletPubKey) $ bob
      amount    = testContractAmount
      startT    = testContractStartTime
      endT      = testContractEndTime

runTrace1 ::EmulatorTrace ()
runTrace1 = do
    aliceStartHdl <- activateContractWallet alice startEscrowEndpoint
    bobHdl <- activateContractWallet bob useEscrowEndpoints

    let param = buildPublishParam
    void $ Emulator.waitNSlots 1
    callEndpoint @"publish" aliceStartHdl param
    tt <- getTT aliceStartHdl

    void $ Emulator.waitNSlots 1
    let useParam = buildUseParam tt
    callEndpoint @"accept" bobHdl useParam

    void $ Emulator.waitNSlots 1

-- Provider shouldn't be able to collect funds before  start time
runTrace2 ::EmulatorTrace ()
runTrace2 = do
    aliceStartHdl <- activateContractWallet alice startEscrowEndpoint
    bobHdl <- activateContractWallet bob useEscrowEndpoints
    aliceHdl <- activateContractWallet alice collectEscrowEndpoint

    let param = buildPublishParam
    void $ Emulator.waitNSlots 1
    callEndpoint @"publish" aliceStartHdl param
    tt <- getTT aliceStartHdl

    void $ Emulator.waitNSlots 1
    let useParam = buildUseParam tt
    callEndpoint @"accept" bobHdl useParam

    void $ Emulator.waitNSlots 2
    callEndpoint @"collect" aliceHdl useParam
    void $ Emulator.waitNSlots 1

-- Provider collects first tranche but cannot collect second tranche right away
runTrace3 ::EmulatorTrace ()
runTrace3 = do
    aliceStartHdl <- activateContractWallet alice startEscrowEndpoint
    bobHdl <- activateContractWallet bob useEscrowEndpoints
    aliceHdl <- activateContractWallet alice collectEscrowEndpoint

    let param = buildPublishParam
    void $ Emulator.waitNSlots 1
    callEndpoint @"publish" aliceStartHdl param
    tt <- getTT aliceStartHdl

    void $ Emulator.waitNSlots 1
    let useParam = buildUseParam tt
    callEndpoint @"accept" bobHdl useParam

    void $ Emulator.waitUntilSlot 10
    callEndpoint @"collect" aliceHdl useParam
    void $ Emulator.waitNSlots 1
    callEndpoint @"collect" aliceHdl useParam
    void $ Emulator.waitNSlots 1
    callEndpoint @"collect" aliceHdl useParam	

-- Provider collects all tranches at the eligible intervals
runTrace4 ::EmulatorTrace ()
runTrace4 = do
    aliceStartHdl <- activateContractWallet alice startEscrowEndpoint
    bobHdl <- activateContractWallet bob useEscrowEndpoints
    aliceHdl <- activateContractWallet alice collectEscrowEndpoint

    let param = buildPublishParam
    void $ Emulator.waitNSlots 1
    callEndpoint @"publish" aliceStartHdl param
    tt <- getTT aliceStartHdl

    void $ Emulator.waitNSlots 1
    let useParam = buildUseParam tt
    callEndpoint @"accept" bobHdl useParam

    void $ Emulator.waitUntilSlot 10
    callEndpoint @"collect" aliceHdl useParam
    void $ Emulator.waitNSlots 4
    callEndpoint @"collect" aliceHdl useParam
    void $ Emulator.waitNSlots 4
    callEndpoint @"collect" aliceHdl useParam
    void $ Emulator.waitNSlots 4
    callEndpoint @"collect" aliceHdl useParam
    void $ Emulator.waitNSlots 4

-- Once consumer raises dispute (after tranche2) the provider cannot collect funds, consumer cannot close within three days
runTrace5 ::EmulatorTrace ()
runTrace5 = do
    aliceStartHdl <- activateContractWallet alice startEscrowEndpoint
    bobHdl <- activateContractWallet bob useEscrowEndpoints
    aliceHdl <- activateContractWallet alice collectEscrowEndpoint

    let param = buildPublishParam
    void $ Emulator.waitNSlots 1
    callEndpoint @"publish" aliceStartHdl param
    tt <- getTT aliceStartHdl

    void $ Emulator.waitNSlots 1
    let useParam = buildUseParam tt
    callEndpoint @"accept" bobHdl useParam

    void $ Emulator.waitUntilSlot 10
    callEndpoint @"collect" aliceHdl useParam
    void $ Emulator.waitNSlots 4
    callEndpoint @"collect" aliceHdl useParam
    void $ Emulator.waitNSlots 4
    callEndpoint @"dispute" bobHdl useParam
    void $ Emulator.waitNSlots 1
    callEndpoint @"collect" aliceHdl useParam
    void $ Emulator.waitUntilSlot 125
    callEndpoint @"close" bobHdl useParam

-- Disputed after first tranche, consumer can claim back funds after max settlement time - 3 days after end time, has elapsed
runTrace6 ::EmulatorTrace ()
runTrace6 = do
    aliceStartHdl <- activateContractWallet alice startEscrowEndpoint
    bobHdl <- activateContractWallet bob useEscrowEndpoints
    aliceHdl <- activateContractWallet alice collectEscrowEndpoint

    let param = buildPublishParam
    void $ Emulator.waitNSlots 1
    callEndpoint @"publish" aliceStartHdl param
    tt <- getTT aliceStartHdl

    void $ Emulator.waitNSlots 1
    let useParam = buildUseParam tt
    callEndpoint @"accept" bobHdl useParam

    void $ Emulator.waitUntilSlot 10
    callEndpoint @"collect" aliceHdl useParam
    void $ Emulator.waitNSlots 4
    callEndpoint @"dispute" bobHdl useParam
    void $ Emulator.waitNSlots 4
    callEndpoint @"collect" aliceHdl useParam
    void $ Emulator.waitUntilSlot 126
    callEndpoint @"close" bobHdl useParam

-- Disputed after first tranche, but consumer unblocks after second tranche. Alice can collect all tranches then.
runTrace7 ::EmulatorTrace ()
runTrace7 = do
    aliceStartHdl <- activateContractWallet alice startEscrowEndpoint
    bobHdl <- activateContractWallet bob useEscrowEndpoints
    aliceHdl <- activateContractWallet alice collectEscrowEndpoint

    let param = buildPublishParam
    void $ Emulator.waitNSlots 1
    callEndpoint @"publish" aliceStartHdl param
    tt <- getTT aliceStartHdl

    void $ Emulator.waitNSlots 1
    let useParam = buildUseParam tt
    callEndpoint @"accept" bobHdl useParam

    void $ Emulator.waitUntilSlot 10
    callEndpoint @"collect" aliceHdl useParam
    void $ Emulator.waitNSlots 1
    callEndpoint @"dispute" bobHdl useParam
    void $ Emulator.waitNSlots 60
    callEndpoint @"unblock" bobHdl useParam
    void $ Emulator.waitNSlots 1
    callEndpoint @"collect" aliceHdl useParam
    void $ Emulator.waitNSlots 1
    callEndpoint @"collect" aliceHdl useParam
    void $ Emulator.waitNSlots 1
    callEndpoint @"collect" aliceHdl useParam
