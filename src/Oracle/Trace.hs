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

testToken :: IO ()
testToken = runEmulatorTraceIO oracleTrace

alice, bob :: Wallet
alice = X.knownWallet 1
bob = X.knownWallet 2

oracleTrace :: EmulatorTrace ()
oracleTrace = do
    h <- activateContractWallet alice startEndpoint
    callEndpoint @"start" h ()
   {-void $ Emulator.waitNSlots 5
    Last m <- observableState h
    case m of
        Nothing -> Extras.logError @String "error starting token sale"
        Just ts -> do
            Extras.logInfo $ "started token sale " ++ show ts
            return ()-}


            {-h1 <- activateContractWallet w1 $ useEndpoints ts
            h2 <- activateContractWallet w2 $ useEndpoints ts
            h3 <- activateContractWallet w3 $ useEndpoints ts

            callEndpoint @"set price" h1 1_000_000
            void $ Emulator.waitNSlots 5

            callEndpoint @"add tokens" h1 100
            void $ Emulator.waitNSlots 5

            callEndpoint @"buy tokens" h2 20
            void $ Emulator.waitNSlots 5

            callEndpoint @"buy tokens" h3 5
            void $ Emulator.waitNSlots 5

            callEndpoint @"close" h1 ()
            void $ Emulator.waitNSlots 5-}