module ContractProperties
    where

import           PlutusTx.Builtins.Class     (stringToBuiltinByteString)
import           Ledger.Value                as Value
import           Oracle.OnChain

markerTokenNameStr :: String
markerTokenNameStr = "TADROrcl"

contractTokenName :: TokenName
contractTokenName = TokenName { unTokenName = stringToBuiltinByteString markerTokenNameStr}

markerValue :: Value
markerValue = Value.singleton (markerCurSymbol contractTokenName) contractTokenName 1

contractOracle :: Oracle
contractOracle = Oracle {oSymbol = markerCurSymbol contractTokenName, tn = contractTokenName}

