{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module PAB
    ( OracleContracts (..)
    ) where

import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.OpenApi.Schema                 (ToSchema)
import           GHC.Generics                        (Generic)
import qualified Ledger
import           Plutus.PAB.Effects.Contract.Builtin (Empty, HasDefinitions (..), SomeBuiltin (..), endpointsToSchemas)
import           Prettyprinter                       (Pretty (..), viaShow)
import           Wallet.Emulator.Wallet              (knownWallet, mockWalletAddress)

import qualified Oracle.OffChain               as Oracle

data OracleContracts = OracleStart Ledger.PaymentPubKeyHash
                     | OracleUpdate Ledger.PaymentPubKeyHash
                     | OracleInspect
                     | SendRewards
                     | PayToTheScript Integer
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, ToSchema)

instance Pretty OracleContracts where
    pretty = viaShow

instance HasDefinitions OracleContracts where

    getDefinitions        = [OracleStart examplePkh, OracleUpdate examplePkh, OracleInspect, SendRewards, PayToTheScript exampleAmt]

    getContract (OracleStart pkh)      = SomeBuiltin $ Oracle.startOracle @() @Empty pkh
    getContract (OracleUpdate pkh)      = SomeBuiltin $ Oracle.updateOracle @() @Empty pkh
    getContract (OracleInspect)      = SomeBuiltin $ Oracle.inspectOracle @() @Empty
    getContract (SendRewards)      = SomeBuiltin $ Oracle.payRewards @() @Empty
    getContract (PayToTheScript amt) = SomeBuiltin $ Oracle.payToTheScript @() @Empty amt

    getSchema = const $ endpointsToSchemas @Empty

examplePkh :: Ledger.PaymentPubKeyHash
examplePkh =  Ledger.PaymentPubKeyHash "75e4c9eb934b31cb014e37109be742ea298dcf8aa68797602ec0a9b0"

exampleAmt :: Integer
exampleAmt =  1010
