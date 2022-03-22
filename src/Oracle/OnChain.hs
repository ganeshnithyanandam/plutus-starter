{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Oracle.OnChain where

import           Data.String (fromString)
import           Data.Maybe                  (fromJust)

import qualified PlutusTx
import           PlutusTx.Builtins.Class (stringToBuiltinByteString)
import           PlutusTx.IsData.Class
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           Ledger                      hiding (mint, singleton)
import qualified Ledger.Typed.Scripts        as Scripts
import           Ledger.Value                as Value
import           Control.Monad             hiding (fmap)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Map                  as Map
import           Data.Monoid               (Last (..))
import           Data.Text                 (Text, pack)
import           GHC.Generics              (Generic)
import           Plutus.Contract           as Contract hiding (when)
import           Plutus.Contract.Wallet      (getUnspentOutput)
import qualified PlutusTx
import           Ledger                    hiding (singleton)
import           Ledger.Constraints        as Constraints
import           Ledger.Value              as Value
import           Ledger.Ada                as Ada
import           Prelude                   (Semigroup (..), Show(..), String)
import qualified Prelude                   as Prelude
import           Text.Printf                 (printf)



{-# INLINABLE mkMarkerPolicy #-}
mkMarkerPolicy :: TokenName -> () -> ScriptContext -> Bool
mkMarkerPolicy tn () ctx = traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn', amt')] -> tn' == tn && amt' == 1
        _               -> False

markerPolicy :: TokenName -> Scripts.MintingPolicy
markerPolicy tn = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \tn'->  Scripts.wrapMintingPolicy $ mkMarkerPolicy tn' ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode tn

{-# INLINABLE markerCurSymbol #-}
markerCurSymbol :: TokenName -> CurrencySymbol
markerCurSymbol = scriptCurrencySymbol . markerPolicy

{-# INLINABLE markerAsset #-}
markerAsset :: TokenName -> AssetClass
markerAsset tn = AssetClass (markerCurSymbol tn, tn)

data Oracle = Oracle
      { oSymbol :: !CurrencySymbol
      , tn :: !TokenName
      } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''Oracle

data OracleStatus = Used | Unused
  deriving (Show, Eq)

data OracleDatum = OracleDatum  { status ::  OracleStatus
                                , pkh :: PaymentPubKeyHash
                                }
  deriving (Show, Eq)

data OracleRedeemer = Use | Update
  deriving (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed ''OracleStatus [('Used,0), ('Unused,1)]
PlutusTx.makeIsDataIndexed ''OracleDatum [('OracleDatum,0)]
PlutusTx.makeIsDataIndexed ''OracleRedeemer [('Use,0), ('Update,1)]

{-# INLINABLE mkOracleValidator #-}
mkOracleValidator :: Oracle -> OracleDatum -> OracleRedeemer -> ScriptContext -> Bool
mkOracleValidator orcl dat r ctx =
    traceIfFalse "Marker token missing from input"  inputHasToken  &&
    traceIfFalse "Marker token missing from output" outputHasMarker &&
    case r of
        Update -> True
        Use    -> True
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "oracle input missing"
        Just i  -> txInInfoResolved i

    inputHasToken :: Bool
    inputHasToken = assetClassValueOf (txOutValue ownInput) (oracleAsset orcl) == 1

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one oracle output"

    outputHasMarker :: Bool
    outputHasMarker = assetClassValueOf (txOutValue ownOutput) (oracleAsset orcl) == 1


data Oracling
instance Scripts.ValidatorTypes Oracling where
    type instance DatumType Oracling = OracleDatum
    type instance RedeemerType Oracling = OracleRedeemer

oracleScriptInst :: Oracle -> Scripts.TypedValidator Oracling
oracleScriptInst o = Scripts.mkTypedValidator @Oracling
    ($$(PlutusTx.compile [|| mkOracleValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode o)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @OracleDatum @OracleRedeemer

oracleValScript :: Oracle -> Validator
oracleValScript = Scripts.validatorScript . oracleScriptInst

oracleValHash :: Oracle -> Ledger.ValidatorHash
oracleValHash = Scripts.validatorHash . oracleScriptInst

oracleAddress :: Oracle -> Ledger.Address
oracleAddress = scriptAddress . oracleValScript

{-# INLINABLE oracleAsset #-}
oracleAsset :: Oracle -> AssetClass
oracleAsset oracle = AssetClass (oSymbol oracle, tn oracle)

