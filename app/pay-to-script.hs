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
import Utils                      (contractActivationArgs, unsafeReadWalletId, unsafeReadInteger)

main :: IO ()
main = do
    [wid', amt'] <- getArgs
    let wid = unsafeReadWalletId wid'
        amt = read amt' :: Integer
    printf "Adding lovelaces to the script \n"
    cid <- payToTheScript wid amt
    printf "Added lovelaces: %s\n" $ show amt

payToTheScript :: WalletId -> Integer -> IO ContractInstanceId
payToTheScript wid llaces = do
    v <- runReq defaultHttpConfig $ req
        POST
        (http "127.0.0.1" /: "api"  /: "contract" /: "activate")
        (ReqBodyJson $ contractActivationArgs wid $ PayToTheScript llaces)
        jsonResponse
        (port 9080)
    let c = responseStatusCode v
    if c == 200
        then return $ responseBody v
        else throwIO $ userError $ printf "ERROR: %d\n" c
