{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE TypeApplications   #-}

module RSC.OffChain (
    PayFromScriptToAddress
    , PayToWalletParams(..)
    , PayToWalletSchema
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Void (Void)
import GHC.Generics (Generic)
import Schema (ToSchema)

import Ledger (PaymentPubKeyHash, Value)
import Ledger.Constraints (adjustUnbalancedTx, mustPayToPubKey)
import Plutus.Contract (ContractError, Endpoint, Promise, endpoint, logInfo, mkTxConstraints, yieldUnbalancedTx)

import Oracle.OnChain

data PayToWalletParams =
    PayToWalletParams
        { amount :: Value
        , pkh    :: PaymentPubKeyHash
        }
        deriving stock (Eq, Show, Generic)
        deriving anyclass (ToJSON, FromJSON, ToSchema)

type PayToWalletSchema = Endpoint "PayFromScriptToAddress" PayToWalletParams

payFromScriptToAddress :: Promise () PayToWalletSchema ContractError ()
payFromScriptToAddress = endpoint @"PayFromScriptToAddress" $ do
    logInfo @String "Calling PayFromScriptToAddress endpoint"
    utx <- mkTxConstraints @Void mempty (mustPayToPubKey pkh amount)
    logInfo @String $ "Yielding the unbalanced transaction " <> show utx
    yieldUnbalancedTx $ adjustUnbalancedTx utx

