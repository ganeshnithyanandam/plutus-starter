{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}

module Oracle.OffChain
    ( startOracle
    , startEndpoint
    ) where

import           Control.Monad               hiding (fmap)
import           PlutusTx.IsData.Class
import           Data.Aeson                  (FromJSON, ToJSON)
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromJust)
import           Data.OpenApi.Schema         (ToSchema)
import           Data.Text                   (Text, pack)
import           Data.Void                   (Void)
import           GHC.Generics                (Generic)
import           Plutus.Contract             as Contract
import           Plutus.Contract.Wallet      (getUnspentOutput)
import qualified PlutusTx
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           Ledger                      hiding (mint, singleton)
import           Ledger.Constraints          as Constraints
import qualified Ledger.Typed.Scripts        as Scripts
import           Ledger.Value                as Value
import           Prelude                     (Semigroup (..), Show (..), String)
import qualified Prelude
import           Text.Printf                 (printf)

import           Oracle.OnChain
import           Utils                (getCredentials)

startOracle :: Contract w s Text ()
startOracle = do
            oref <- getUnspentOutput
            o    <- fromJust <$> Contract.unspentTxOutFromRef oref
            Contract.logDebug @String $ printf "picked UTxO at %s with value %s" (show oref) (show $ _ciTxOutValue o)

            let tn' = (TokenName { unTokenName = "ADROrcl" })
                orcl        = Oracle {oSymbol = markerCurSymbol tn', tn = tn'}
                val         = Value.singleton (markerCurSymbol "ADROrcl") "ADROrcl" 1
                lookups     = Constraints.mintingPolicy (markerPolicy (TokenName { unTokenName = "ADROrcl" })) <>
                              Constraints.unspentOutputs (Map.singleton oref o) <>
                              Constraints.otherScript (oracleValScript orcl)
                constraints = Constraints.mustMintValue val          <>
                              Constraints.mustSpendPubKeyOutput oref <>
                              Constraints.mustPayToOtherScript (oracleValHash orcl) (Datum $ toBuiltinData Unused) val
            {-ledgerTx <- submitTxConstraintsWith @Scripts.Any lookups constraints
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx-}
            void $ adjustAndSubmitWith @Scripts.Any lookups constraints
            Contract.logInfo @String $ printf "minted %s" (show val)


adjustAndSubmit :: ( PlutusTx.FromData (Scripts.DatumType a)
                   , PlutusTx.ToData (Scripts.RedeemerType a)
                   , PlutusTx.ToData (Scripts.DatumType a)
                   , AsContractError e
                   )
                => Scripts.TypedValidator a
                -> TxConstraints (Scripts.RedeemerType a) (Scripts.DatumType a)
                -> Contract w s e CardanoTx
adjustAndSubmit inst = adjustAndSubmitWith $ Constraints.typedValidatorLookups inst

adjustAndSubmitWith :: ( PlutusTx.FromData (Scripts.DatumType a)
                       , PlutusTx.ToData (Scripts.RedeemerType a)
                       , PlutusTx.ToData (Scripts.DatumType a)
                       , AsContractError e
                       )
                    => ScriptLookups a
                    -> TxConstraints (Scripts.RedeemerType a) (Scripts.DatumType a)
                    -> Contract w s e CardanoTx
adjustAndSubmitWith lookups constraints = do
    unbalanced <- adjustUnbalancedTx <$> mkTxConstraints lookups constraints
    Contract.logDebug @String $ printf "unbalanced: %s" $ show unbalanced
    unsigned <- balanceTx unbalanced
    Contract.logDebug @String $ printf "balanced: %s" $ show unsigned
    signed <- submitBalancedTx unsigned
    Contract.logDebug @String $ printf "signed: %s" $ show signed
    return signed

type OracleSchema = Endpoint "start" ()

startEndpoint :: Contract () OracleSchema Text ()
startEndpoint = forever
              $ handleError logError
              $ awaitPromise
              $ endpoint @"start" $ \ _ -> do startOracle


