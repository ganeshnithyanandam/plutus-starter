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

startOracle :: PaymentPubKeyHash -> Contract w s Text ()
startOracle pkh = do
            oref <- getUnspentOutput
            o    <- fromJust <$> Contract.unspentTxOutFromRef oref
            Contract.logDebug @String $ printf "picked UTxO at %s with value %s" (show oref) (show $ _ciTxOutValue o)

            let tn' = (TokenName { unTokenName = "ADROrcl" })
                orcl        = Oracle {oSymbol = markerCurSymbol tn', tn = tn'}
                markerVal         = Value.singleton (markerCurSymbol "ADROrcl") "ADROrcl" 1
                lookups     = Constraints.mintingPolicy (markerPolicy (TokenName { unTokenName = "ADROrcl" })) <>
                              Constraints.unspentOutputs (Map.singleton oref o) <>
                              Constraints.otherScript (oracleValScript orcl)
                constraints = Constraints.mustMintValue markerVal
                              <> Constraints.mustSpendPubKeyOutput oref
                              <> Constraints.mustPayToOtherScript (oracleValHash orcl) (Datum $ toBuiltinData $ oracleDatumWith Unused pkh) markerVal
            void $ adjustAndSubmitWith @Scripts.Any lookups constraints
            Contract.logInfo @String $ printf "minted %s" (show markerVal)


updateOracle :: PaymentPubKeyHash -> Contract w s Text ()
updateOracle pkh = do
            let tn' = (TokenName { unTokenName = "ADROrcl" })
                orcl        = Oracle {oSymbol = markerCurSymbol tn', tn = tn'}
                markerVal   = Value.singleton (markerCurSymbol "ADROrcl") "ADROrcl" 1
                dat         = oracleDatumWith Used pkh
                lookups     = Constraints.typedValidatorLookups (oracleScriptInst orcl)
                constraints = Constraints.mustPayToTheScript dat markerVal
            void $ adjustAndSubmitWith @Oracling lookups constraints
            Contract.logInfo @String $ printf "Update oracle with data: %s" (show $ dat)

oracleDatumWith :: OracleStatus -> PaymentPubKeyHash -> OracleDatum
oracleDatumWith s' pkh' = OracleDatum {status = s', pkh = pkh'}

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
            let tn' = (TokenName { unTokenName = "ADROrcl" })
                orcl = Oracle {oSymbol = markerCurSymbol tn', tn = tn'}
            os  <- map snd . Map.toList <$> utxosAt (oracleAddress orcl)
            let val = mconcat [view ciTxOutValue o | o <- os]
                markerOs = [o | o <- os, csMatcher (markerCurSymbol tn') (view ciTxOutValue o)]
                markerDatum = head $ [datumContent o | o <- markerOs]
            Contract.logInfo @String $ printf "Outputs at oracle %s" $ show os
            Contract.logInfo @String $ printf "Total value at oracle %s" $ show val
            Contract.logInfo @String $ printf "Marker outputs at oracle %s" $ show markerOs
            Contract.logInfo @String $ printf "Datum at oracle %s" $ show markerDatum
            return markerDatum

markerVal :: Value
markerVal = Value.singleton (markerCurSymbol "ADROrcl") "ADROrcl" 1

csMatcher :: CurrencySymbol -> Value -> Bool
csMatcher cs val = cs `elem` symbols val

datumContent :: ChainIndexTxOut -> Maybe OracleDatum
datumContent o = do
  Datum d <- either (const Nothing) Just (_ciTxOutDatum o)
  PlutusTx.fromBuiltinData d

type OracleSchema = Endpoint "start" PaymentPubKeyHash
                    .\/ Endpoint "update" PaymentPubKeyHash
                    .\/ Endpoint "inspect" ()

useOrclEndpoints :: Contract () OracleSchema Text ()
useOrclEndpoints = forever
              $ handleError logError
              $ awaitPromise
              $ start' `select` udpate'
              where
                start'   = endpoint @"start" (\x -> startOracle x)
                udpate'  = endpoint @"update" (\x -> updateOracle x)

inspectEndpoint :: Promise () OracleSchema Text (Maybe OracleDatum)
inspectEndpoint = endpoint @"inspect" $ \_ -> do inspectOracle

payRewards :: Contract w s Text ()
payRewards = do
        dat <- inspectOracle
        Contract.logDebug @String $ printf "Received datum from oracle: %s" (show dat)
        let tn' = (TokenName { unTokenName = "ADROrcl" })
            orcl = Oracle {oSymbol = markerCurSymbol tn', tn = tn'}
            pkh' = pkh $ fromJust dat
        os  <- map snd . Map.toList <$> utxosAt (oracleAddress orcl)
        utxos <- utxosAt (oracleAddress orcl)
        let val = mconcat [view ciTxOutValue o | o <- os, csMatcher (markerCurSymbol tn') (view ciTxOutValue o) == False]
            lookups = Constraints.typedValidatorLookups (oracleScriptInst orcl)
                      <> Constraints.unspentOutputs utxos
            constraints = collectFromScript utxos (Update)
                          <> Constraints.mustPayToPubKey (pkh') val
                          <> Constraints.mustPayToTheScript (oracleDatumWith Used pkh') markerVal
        void $ adjustAndSubmitWith @Oracling lookups constraints
        Contract.logInfo @String $ printf "Rewards paid with data: %s" (show $ dat)

type WalletSchema = Endpoint "payRewards" ()

walletEndpoint :: Contract () WalletSchema Text ()
walletEndpoint = forever
              $ handleError logError
              $ awaitPromise
              $ payRewards'
                where
                  payRewards'   = endpoint @"payRewards" $ \_ -> do payRewards


