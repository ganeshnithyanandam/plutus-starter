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
import Utils                      (contractActivationArgs, unsafeReadWalletId)

main :: IO ()
main = do
    [wid'] <- getArgs
    let wid = unsafeReadWalletId wid'
    printf "Sending rewards to pkh at the oracle \n"
    cid <- sendRewards wid
    printf "Rewards sent \n" $ show cid

sendRewards :: WalletId -> IO ContractInstanceId
sendRewards wid = do
    v <- runReq defaultHttpConfig $ req
        POST
        (http "127.0.0.1" /: "api"  /: "contract" /: "activate")
        (ReqBodyJson $ contractActivationArgs wid SendRewards)
        jsonResponse
        (port 9080)
    let c = responseStatusCode v
    if c == 200
        then return $ responseBody v
        else throwIO $ userError $ printf "ERROR: %d\n" c
