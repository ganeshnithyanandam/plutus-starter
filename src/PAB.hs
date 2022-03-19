{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module PAB
    ( Address
    , RSContracts (..)
    ) where

import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.OpenApi.Schema                 (ToSchema)
import           GHC.Generics                        (Generic)
import           Ledger                              (Address)
import           Plutus.PAB.Effects.Contract.Builtin (Empty, HasDefinitions (..), SomeBuiltin (..), endpointsToSchemas)
import           Prettyprinter                       (Pretty (..), viaShow)
import           Wallet.Emulator.Wallet              (knownWallet, mockWalletAddress)

import qualified Monitor                      as Monitor
import qualified Token.OffChain               as Token

data RSContracts = Mint Token.TokenParams | Monitor Address
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, ToSchema)

instance Pretty RSContracts where
    pretty = viaShow

instance HasDefinitions RSContracts where

    getDefinitions        = [Mint exampleTP, Monitor exampleAddr]

    getContract (Mint tp)      = SomeBuiltin $ Token.mintToken @() @Empty tp
    getContract (Monitor addr) = SomeBuiltin $ Monitor.monitor addr

    getSchema = const $ endpointsToSchemas @Empty

exampleAddr :: Address
exampleAddr = mockWalletAddress $ knownWallet 1

exampleTP :: Token.TokenParams
exampleTP = Token.TokenParams
    { Token.tpAddress = exampleAddr
    , Token.tpAmount  = 123456
    , Token.tpToken   = "PPP"
    }
