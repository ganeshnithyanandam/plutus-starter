{-# LANGUAGE OverloadedStrings  #-}

module Main
    ( main
    ) where

import Control.Exception          (throwIO)
import Data.String                (IsString (..))
import qualified Ledger
import Network.HTTP.Req
import System.Environment         (getArgs)
import Text.Printf                (printf)
import Wallet.Emulator.Wallet     (WalletId (..))
import Wallet.Types               (ContractInstanceId (..))
import PAB                        (OracleContracts (..))
import Utils                      (contractActivationArgs, unsafeReadAddress, unsafeReadWalletId, unsafePaymentPubKeyHash)

main :: IO ()
main = do
    [wid', addr'] <- getArgs
    let wid = unsafeReadWalletId wid'
    let pkh = unsafePaymentPubKeyHash $ unsafeReadAddress addr'
    printf "Updating oracle with datum pkh as %s\n" (show pkh)
    cid <- updateOracle wid pkh
    printf "Oracle updated, contract instance id: %s\n" $ show cid

updateOracle :: WalletId -> Ledger.PaymentPubKeyHash -> IO ContractInstanceId
updateOracle wid pkh = do
    v <- runReq defaultHttpConfig $ req
        POST
        (http "127.0.0.1" /: "api"  /: "contract" /: "activate")
        (ReqBodyJson $ contractActivationArgs wid $ OracleUpdate pkh)
        jsonResponse
        (port 9080)
    let c = responseStatusCode v
    if c == 200
        then return $ responseBody v
        else throwIO $ userError $ printf "ERROR: %d\n" c
