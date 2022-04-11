{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Oracle.Deploy
    ( writeJSON
    , writeValidator
    , writeUnit
    , writeOracleValidator
    , writeMarkerMintingPolicy
    , writeUnusedDatum
    , writeUsedDatum
    , writeUseRedeemer
    , writeUpdateRedeemer
    ) where

import           Cardano.Api
import           Cardano.Api.Shelley   (PlutusScript (..))
import           Codec.Serialise       (serialise)
import           Data.Aeson            (encode)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           PlutusTx              (Data (..))
import qualified PlutusTx
import qualified Ledger
import           Ledger.Value                as Value

import           Oracle.OnChain
import           Oracle.OffChain
import           ContractProperties
import           Utils                  (unsafePaymentPubKeyHash, unsafeReadAddress)

dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript

writeUnit :: IO ()
writeUnit = writeJSON "deploy/testnet/unit.json" ()

writeUnusedDatum :: IO ()
writeUnusedDatum = writeJSON "deploy/testnet/datum-unused-pkh.json" $
 oracleDatumWith Unused (Ledger.PaymentPubKeyHash "75e4c9eb934b31cb014e37109be742ea298dcf8aa68797602ec0a9b0")

writeUsedDatum :: IO ()
writeUsedDatum = writeJSON "deploy/testnet/datum-used-pkh.json" $
 oracleDatumWith Used (Ledger.PaymentPubKeyHash "75e4c9eb934b31cb014e37109be742ea298dcf8aa68797602ec0a9b0")

writeUseRedeemer :: IO ()
writeUseRedeemer = writeJSON "deploy/testnet/rdr-use-oracle.json" Use

writeUpdateRedeemer :: IO ()
writeUpdateRedeemer = writeJSON "deploy/testnet/rdr-update-oracle.json" Update

writeOracleValidator :: String -> IO (Either (FileError ()) ())
writeOracleValidator addr =
   writeOracleValidator' $ unsafePaymentPubKeyHash $ unsafeReadAddress addr

writeOracleValidator' :: Ledger.PaymentPubKeyHash -> IO (Either (FileError ()) ())
writeOracleValidator' wPkh =
  let tn'   = contractTokenName
      orcl  = contractOracle wPkh
  in writeValidator "deploy/testnet/oracle.plutus" $ oracleValScript orcl

writeMarkerMintingPolicy :: IO (Either (FileError ()) ())
writeMarkerMintingPolicy =
  let tName   = contractTokenName
  in writeFileTextEnvelope @(PlutusScript PlutusScriptV1)
                            "deploy/testnet/mintmarker.plutus"
                            Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.getMintingPolicy $ markerPolicy tName

