{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Oracle.OffChain where

import           Control.Lens (view)
import           Control.Monad               hiding (fmap)
import           Data.Aeson                  (FromJSON, ToJSON)
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromJust)
import           Data.OpenApi.Schema         (ToSchema)
import           Data.Text                   (Text, pack)
import           Data.Void                   (Void)
import           GHC.Generics                (Generic)
import           Ledger                      hiding (mint, singleton)
import           Ledger.Ada                  (lovelaceValueOf)
import           Ledger.Constraints          as Constraints
import qualified Ledger.Typed.Scripts        as Scripts
import           Ledger.Value                as Value
import           Plutus.Contract             as Contract
import           Plutus.Contract.Request     as Request
import           Plutus.Contract.Wallet      (getUnspentOutput)
import           Plutus.V1.Ledger.Ada        (fromValue, toValue)
import qualified PlutusTx
import           PlutusTx.IsData.Class
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           Prelude                     (Semigroup (..), Show (..), String)
import qualified Prelude
import           Text.Printf                 (printf)

import           Oracle.OnChain
import           Utils                (getCredentials)
import           ContractProperties

startOracle :: PaymentPubKeyHash -> Contract w s Text ()
startOracle pkh = do
            oref <- getUnspentOutput
            Contract.logInfo @String $ printf "picked UTxO %s" (show oref)
            o    <- fromJust <$> Contract.txOutFromRef oref
            Contract.logDebug @String $ printf "picked UTxO at %s with value %s" (show oref) (show $ _ciTxOutValue o)

            let tn' = contractTokenName
                orcl        = contractOracle
                markerValue   = Value.singleton (markerCurSymbol "TADROrcl") "TADROrcl" 1
                lookups     = Constraints.mintingPolicy (markerPolicy contractTokenName) <>
                              Constraints.unspentOutputs (Map.singleton oref o) <>
                              Constraints.otherScript (oracleValScript orcl)
                constraints = Constraints.mustMintValue markerValue
                              <> Constraints.mustSpendPubKeyOutput oref
                              <> Constraints.mustPayToOtherScript (oracleValHash orcl) (Datum $ toBuiltinData $ oracleDatumWith Unused pkh) markerValue
            void $ adjustAndSubmitWith @Scripts.Any lookups constraints
            Contract.logInfo @String $ printf "minted %s" (show markerValue)


updateOracle :: PaymentPubKeyHash -> Contract w s Text ()
updateOracle pkh = do
            let tn' = contractTokenName
                orcl        = contractOracle
                markerValue   = Value.singleton (markerCurSymbol "TADROrcl") "TADROrcl" 1
                dat         = oracleDatumWith Unused pkh
            scrUtxos  <- utxosAt (oracleAddress orcl)
            let markerUtxo = Map.filter (\x -> csMatcher (markerCurSymbol tn') $ view ciTxOutValue x) scrUtxos
            --let markerO = [uxo | uxo <- scrUtxos, csMatcher (markerCurSymbol tn') (view ciTxOutValue (map snd . Map.toList <$> scrUtxos))]
            let lookups     = Constraints.typedValidatorLookups (oracleScriptInst orcl)
                              <> Constraints.otherScript (oracleValScript orcl)
                              <> Constraints.unspentOutputs (markerUtxo)
                constraints = Constraints.mustPayToTheScript dat markerValue
                              <> Constraints.mustSpendScriptOutput (fst (head $ Map.toList markerUtxo)) (Redeemer $ PlutusTx.toBuiltinData Update)
            void $ adjustAndSubmitWith @Oracling lookups constraints
            Contract.logInfo @String $ printf "Update oracle with data: %s" (show $ dat)

oracleDatumWith :: OracleStatus -> PaymentPubKeyHash -> OracleDatum
oracleDatumWith s' pkh' = OracleDatumMarker {status = s', pkh = pkh'}

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

inspectOracle :: Contract w s Text (Maybe OracleDatum)
inspectOracle = do
            Contract.logInfo @String $ printf "Inspecting oracle\n"
            let tn' = contractTokenName
                orcl = contractOracle
            os  <- map snd . Map.toList <$> utxosAt (oracleAddress orcl)
            let val = mconcat [view ciTxOutValue o | o <- os]
                markerOs = [o | o <- os, csMatcher (markerCurSymbol tn') (view ciTxOutValue o)]
            Contract.logInfo @String $ printf "Outputs at oracle %s\n" $ show os
            Contract.logInfo @String $ printf "Marker outputs at oracle %s\n" $ show markerOs
            let markerDatum = head $ [datumContent o | o <- markerOs]
            Contract.logInfo @String $ printf "Total value at oracle %s\n" $ show val
            Contract.logInfo @String $ printf "Datum at oracle %s\n" $ show markerDatum
            return markerDatum

inspectOracleNoReturn :: Contract w s Text ()
inspectOracleNoReturn = do
            Contract.logInfo @String $ printf "Inspecting oracle\n"
            let tn' = contractTokenName
                orcl = contractOracle
            os  <- map snd . Map.toList <$> utxosAt (oracleAddress orcl)
            let val = mconcat [view ciTxOutValue o | o <- os]
                markerOs = [o | o <- os, csMatcher (markerCurSymbol tn') (view ciTxOutValue o)]
            Contract.logInfo @String $ printf "Outputs at oracle %s\n" $ show os
            Contract.logInfo @String $ printf "Marker outputs at oracle %s\n" $ show markerOs
            let markerDatum = head $ [datumContent o | o <- markerOs]
            Contract.logInfo @String $ printf "Total value at oracle %s\n" $ show val
            Contract.logInfo @String $ printf "Datum at oracle %s\n" $ show markerDatum


csMatcher :: CurrencySymbol -> Value -> Bool
csMatcher cs val = cs `elem` symbols val

datumContent :: ChainIndexTxOut -> Maybe OracleDatum
datumContent o = do
  Datum d <- either (const Nothing) Just (_ciTxOutDatum o)
  PlutusTx.fromBuiltinData d

type OracleSchema = Endpoint "start" PaymentPubKeyHash
                    .\/ Endpoint "update" PaymentPubKeyHash
                    .\/ Endpoint "inspect" ()
                    .\/ Endpoint "inspectNR" ()

useOrclEndpoints :: Contract () OracleSchema Text ()
useOrclEndpoints = forever
              $ handleError logError
              $ awaitPromise
              $ start' `select` update'
              where
                start'   = endpoint @"start" (\x -> startOracle x)
                update'  = endpoint @"update" (\x -> updateOracle x)

inspectEndpointNR :: Contract () OracleSchema Text ()
inspectEndpointNR = forever
              $ handleError logError
              $ awaitPromise
              $ inspect'
              where
                inspect'   = endpoint @"inspectNR" (\_ -> inspectOracleNoReturn)

inspectEndpoint :: Promise () OracleSchema Text (Maybe OracleDatum)
inspectEndpoint = endpoint @"inspect" $ \_ -> do inspectOracle

payRewards :: Contract w s Text ()
payRewards = do
        dat <- inspectOracle
        ownPkh <- Request.ownPaymentPubKeyHash
        Contract.logDebug @String $ printf "Received datum from oracle: %s" (show dat)
        let tn' = contractTokenName
            orcl = contractOracle
            pkh' = pkh $ fromJust dat
            status' = status $ fromJust dat
        case status' of
          Used -> Contract.throwError $ pack $ printf "The oracle state is 'Used' for pkh: %s" $ show pkh'
          Unused -> do
            os  <- map snd . Map.toList <$> utxosAt (oracleAddress orcl)
            utxos <- utxosAt (oracleAddress orcl)
            let val = mconcat [view ciTxOutValue o | o <- os, csMatcher (markerCurSymbol tn') (view ciTxOutValue o) == False]
                lookups = Constraints.typedValidatorLookups (oracleScriptInst orcl)
                          <> Constraints.unspentOutputs utxos
                constraints = collectFromScript utxos (Use)
                              <> Constraints.mustPayToPubKey pkh' val
                              <> Constraints.mustPayToTheScript (oracleDatumWith Used pkh') markerValue
            void $ adjustAndSubmitWith @Oracling lookups constraints
            Contract.logInfo @String $ printf "Rewards paid with data: %s" (show $ dat)

payToTheScript :: Integer -> Contract w s Text ()
payToTheScript llaces = do
        Contract.logDebug @String $ printf "Paying to script - lovelaces : %s" (show llaces)
        let orcl = contractOracle
            lookups = Constraints.typedValidatorLookups (oracleScriptInst orcl)
            constraints = Constraints.mustPayToTheScript OracleDatumNone $ lovelaceValueOf llaces
        void $ adjustAndSubmitWith @Oracling lookups constraints
        Contract.logInfo @String $ printf "Paid to %s lovelaces to script" (show llaces)

type WalletSchema = Endpoint "payRewards" ()
                    .\/ Endpoint "payToTheScript" Integer

walletEndpoint :: Contract () WalletSchema Text ()
walletEndpoint = forever
              $ handleError logError
              $ awaitPromise
              $ payRewards' `select` payToTheScript'
                where
                  payRewards' = endpoint @"payRewards" $ \_ -> do payRewards
                  payToTheScript' = endpoint @"payToTheScript" $ \x -> do payToTheScript x


