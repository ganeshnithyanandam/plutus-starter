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

module EscrowTest where

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