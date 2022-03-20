{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module AppliedEscrow
    ( AppliedEscrow (..)
    , EscrowRedeemer (..)
    , PublishParam (..)
    , UseParam (..)
    , StartAppliedEscrowSchema
    , UseAppliedEscrowSchema
    , Last (..)
    , ThreadToken
    , Text
    , startEscrowEndpoint
    , useEscrowEndpoints
    , collectEscrowEndpoint
    , threeDaysTime
    ) where

import           Control.Monad                hiding (fmap)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints hiding (ownPaymentPubKeyHash)
import           Ledger.Constraints.TxConstraints as TxConstraints
import           Ledger.Typed.Tx
import qualified Ledger.Typed.Scripts         as Scripts
import qualified PlutusTx.Builtins            as Builtins
import           Plutus.Contract              as Contract
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import qualified PlutusTx.Prelude             as PP
import           Playground.Contract          (ToSchema)
import           Prelude                      (Semigroup (..), Show (..), String)
import qualified Prelude

data AppliedEscrow = AppliedEscrow
    { provider        :: !PaymentPubKeyHash
    , consumer        :: !PaymentPubKeyHash
    , startTime       :: !POSIXTime
    , endTime         :: !POSIXTime
    , contractValue   :: !Value
    , tranches        :: !Integer
    , eTT             :: !ThreadToken
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''AppliedEscrow

data EscrowRedeemer =
          Accept
        | Withdraw
        | Collect
        | Dispute
        | Unblock
        | Close
        deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

{-Is there a better way to do makeIsDataIndexed ?-}
PlutusTx.makeLift ''EscrowRedeemer
PlutusTx.makeIsDataIndexed ''EscrowRedeemer [('Accept, 0), ('Collect, 1), ('Withdraw, 2), ('Dispute, 3), ('Unblock, 4), ('Close, 5)]

data EscrowDatum = Published | Active | Withdrawn | Disputed | Closed
    deriving (Show)

instance Eq EscrowDatum where
  (==) Published Published = True
  (==) Active Active = True
  (==) Disputed Disputed = True
  (==) _ _ = False

PlutusTx.makeIsDataIndexed ''EscrowDatum [('Published, 0), ('Active, 1), ('Withdrawn, 2), ('Disputed, 3), ('Closed, 4)]

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE transition #-}
transition :: AppliedEscrow -> State EscrowDatum -> EscrowRedeemer -> Maybe (TxConstraints Void Void, State EscrowDatum)
transition escrow s r = case (escrow, stateData s,stateValue s, r) of
    (e, _, v, Accept)                 -> Just ( Constraints.mustBeSignedBy (consumer escrow) <>
                                                Constraints.mustValidateIn (to $ startTime escrow)
                                              , State Active $
                                                v <>
                                                contractValue e
                                              )
    (e, Active, v, Collect)           -> Just ( Constraints.mustBeSignedBy (provider e) <>
                                                Constraints.mustValidateIn (from $ payableTime e v) <>
                                                Constraints.mustPayToPubKey (provider e) (trancheValue e)
                                              , State Active $
                                                v <>
                                                negate (trancheValue e)
                                              )
    (e, Active, v, Dispute)           -> Just ( Constraints.mustBeSignedBy (consumer e) <>
                                                Constraints.mustValidateIn (from $ startTime escrow) <>
                                                Constraints.mustValidateIn (to $ endTime escrow)
                                              , State Disputed $
                                                v
                                              )
    (e, Disputed, v, Unblock)         -> Just ( Constraints.mustBeSignedBy (consumer e) <>
                                                Constraints.mustValidateIn (to $ maxSettlementTime e)
                                              , State Active $
                                                v
                                              )
    (e, Active, v, Close)             -> Just ( TxConstraints.mustSatisfyAnyOf
                                                  [Constraints.mustBeSignedBy (provider e), Constraints.mustBeSignedBy (consumer e)] <>
                                                Constraints.mustValidateIn (from $ startTime escrow) <>
                                                Constraints.mustPayToPubKey (consumer e) (v)
                                              , State Closed $
                                                mempty
                                              )
    (e, Disputed, v, Close)           -> Just ( (TxConstraints.mustSatisfyAnyOf
                                                  [Constraints.mustBeSignedBy (provider e), Constraints.mustBeSignedBy (consumer e)]) <>
                                                Constraints.mustValidateIn (from $ maxSettlementTime e) <>
                                                Constraints.mustPayToPubKey (consumer e) (v)
                                              , State Closed $
                                                mempty
                                              )
    _                                 -> Nothing


{-# INLINABLE trancheValue #-}
trancheValue :: AppliedEscrow -> Value
trancheValue e =
  lovelaceValueOf $ PP.divide (lovelaces $ contractValue e) (tranches e)

{-# INLINABLE payableTime #-}
payableTime :: AppliedEscrow -> Value -> POSIXTime
payableTime e v =
  let start = getPOSIXTime $ startTime e; end = getPOSIXTime $ endTime e; totalTime = end - start; totalAmount = lovelaces $ contractValue e; currentAmount = lovelaces v
    in POSIXTime $ end - (Builtins.multiplyInteger totalTime currentAmount) `PP.divide` totalAmount

{-# INLINABLE maxSettlementTime #-}
maxSettlementTime :: AppliedEscrow -> POSIXTime
maxSettlementTime e =
  let end = getPOSIXTime $ endTime e
    in POSIXTime $ end + threeDaysTime

{-# INLINABLE threeDaysTime #-}
threeDaysTime :: Integer
threeDaysTime = 100_000 -- scaling down : hack for testing. Actual is 259_200_000

{-# INLINABLE isClosed #-}
isClosed :: EscrowDatum -> Bool
isClosed Closed = True
isClosed _ = False

{-# INLINABLE escrowStateMachine #-}
escrowStateMachine :: AppliedEscrow -> StateMachine EscrowDatum EscrowRedeemer
escrowStateMachine e = mkStateMachine (Just $ eTT e) (transition e) (isClosed)

{-# INLINABLE mkEscrowValidator #-}
mkEscrowValidator :: AppliedEscrow -> EscrowDatum -> EscrowRedeemer -> ScriptContext -> Bool
mkEscrowValidator escrow = mkValidator $ escrowStateMachine escrow

type EscrowSMType = StateMachine EscrowDatum EscrowRedeemer

escrowTypedValidator :: AppliedEscrow -> Scripts.TypedValidator EscrowSMType
escrowTypedValidator escrow = Scripts.mkTypedValidator @EscrowSMType
    ($$(PlutusTx.compile [|| mkEscrowValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode escrow)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @EscrowDatum @EscrowRedeemer

escrowValidator :: AppliedEscrow -> Validator
escrowValidator = Scripts.validatorScript . escrowTypedValidator

escrowAddress :: AppliedEscrow -> Ledger.Address
escrowAddress = scriptAddress . escrowValidator

escrowClient :: AppliedEscrow -> StateMachineClient EscrowDatum EscrowRedeemer
escrowClient e = mkStateMachineClient $ StateMachineInstance (escrowStateMachine e) (escrowTypedValidator e)

mapError' :: Contract w s SMContractError a -> Contract w s Text a
mapError' = mapError $ pack . show

data PublishParam = PublishParam
    { p   :: !PaymentPubKeyHash
    , c   :: !PaymentPubKeyHash
    , st  :: !POSIXTime
    , et  :: !POSIXTime
    , ll  :: !Integer
    , tc  :: !Integer
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

publish :: forall s. PublishParam -> Contract (Last ThreadToken) s Text ()
publish param = do
  pkh <- ownPaymentPubKeyHash
  tt  <- mapError' getThreadToken
  logInfo @String $ "Logging thread token: " ++ show tt
  let escrow   = AppliedEscrow
          { provider        = pkh
          , consumer        = c param
          , startTime       = st param
          , endTime         = et param
          , contractValue   = lovelaceValueOf $ ll param
          , tranches        = tc param
          , eTT             = tt
          }
      client = escrowClient escrow
  void $ mapError' $ runInitialise client (Published) mempty
  tell $ Last $ Just tt
  logInfo @String $ "Escrow contract published: " ++ show escrow


data UseParam = UseParam
    { up   :: !PaymentPubKeyHash
    , uc   :: !PaymentPubKeyHash
    , ust  :: !POSIXTime
    , uet  :: !POSIXTime
    , ull  :: !Integer
    , utc  :: !Integer
    , uttn :: !ThreadToken
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

useParamToAppliedEscrow :: UseParam -> AppliedEscrow
useParamToAppliedEscrow param =
              AppliedEscrow
                { provider        = up param
                , consumer        = uc param
                , startTime       = ust param
                , endTime         = uet param
                , contractValue   = lovelaceValueOf $ ull param
                , tranches        = utc param
                , eTT             = uttn param
                }

accept :: UseParam -> Contract w s Text ()
accept param = do
  pkh <- ownPaymentPubKeyHash
  let escrow' = useParamToAppliedEscrow param
      escrow  = escrow' {consumer = pkh}
      client  = escrowClient escrow
  state <- mapError' $ getOnChainState client
  case state of
    Nothing   ->  throwError "Nothing found for on chain state"
    Just (onChainState, _)  ->
      do
        let OnChainState{ocsTxOut=TypedScriptTxOut{tyTxOutData=valueInState}} = onChainState
        logInfo @String $ "Escrow contract found in state: " ++ show valueInState
        case valueInState of
          Published -> do
            void $ mapError' $ runStep client $ Accept
            logInfo @String $ "Contract accepted: " ++ show escrow
          _ -> logInfo @String $ "Contract has to be in published state for 'Accept' action"


collect :: UseParam -> Contract w s Text ()
collect param = do
  pkh <- ownPaymentPubKeyHash
  let escrow' = useParamToAppliedEscrow param
      escrow  = escrow' {provider = pkh}
      client = escrowClient escrow
  state <- mapError' $ getOnChainState client
  case state of
    Nothing   ->  throwError "Nothing found for on chain state"
    Just (onChainState, _)  ->
      do
        let OnChainState{ocsTxOut=TypedScriptTxOut{tyTxOutData=valueInState}} = onChainState
        logInfo @String $ "Escrow contract found in state: " ++ show valueInState
        case valueInState of
          Active -> do
            void $ mapError' $ runStep client $ Collect
            logInfo @String $ "Funds collected: " ++ show escrow
          _ -> logInfo @String $ "Contract has to be in active state for 'Collect' action"


dispute :: UseParam -> Contract w s Text ()
dispute param = do
  pkh <- ownPaymentPubKeyHash
  let escrow' = useParamToAppliedEscrow param
      escrow  = escrow' {consumer = pkh}
      client = escrowClient escrow
  state <- mapError' $ getOnChainState client
  case state of
    Nothing   ->  throwError "Nothing found for on chain state"
    Just (onChainState, _)  ->
      do
        let OnChainState{ocsTxOut=TypedScriptTxOut{tyTxOutData=valueInState}} = onChainState
        logInfo @String $ "Escrow contract found in state: " ++ show valueInState
        case valueInState of
          Active -> do
            void $ mapError' $ runStep client $ Dispute
            logInfo @String $ "Dispute registered: " ++ show escrow
          _ -> logInfo @String $ "Contract has to be in active state for 'Dispute' action"


unblock :: UseParam -> Contract w s Text ()
unblock param = do
  pkh <- ownPaymentPubKeyHash
  let escrow' = useParamToAppliedEscrow param
      escrow  = escrow' {consumer = pkh}
      client = escrowClient escrow
  state <- mapError' $ getOnChainState client
  case state of
    Nothing   ->  throwError "Nothing found for on chain state"
    Just (onChainState, _)  ->
      do
        let OnChainState{ocsTxOut=TypedScriptTxOut{tyTxOutData=valueInState}} = onChainState
        logInfo @String $ "Escrow contract found in state: " ++ show valueInState
        case valueInState of
          Disputed -> do
            void $ mapError' $ runStep client $ Unblock
            logInfo @String $ "Status restored to Active: " ++ show escrow
          _ -> logInfo @String $ "Contract has to be in dispute state for 'Unblock' action"


close :: UseParam -> Contract w s Text ()
close param = do
  pkh <- ownPaymentPubKeyHash
  let escrow = useParamToAppliedEscrow param
      client = escrowClient escrow
  state <- mapError' $ getOnChainState client
  case state of
    Nothing   ->  throwError "Nothing found for on chain state"
    Just (onChainState, _)  ->
      do
        let OnChainState{ocsTxOut=TypedScriptTxOut{tyTxOutData=valueInState}} = onChainState
        logInfo @String $ "Escrow contract found in state: " ++ show valueInState
        case valueInState of
            s
              | s `elem` [Active,Disputed] -> do
                  void $ mapError' $ runStep client $ Close
                  logInfo @String $ "Close invoked at state : " ++ show s
              | otherwise -> logInfo @String $ "Contract has to be in active or disputed state for 'Close' action"


type StartAppliedEscrowSchema = Endpoint "publish" PublishParam

type UseAppliedEscrowSchema = Endpoint "accept" UseParam
                          .\/ Endpoint "collect" UseParam
                          .\/ Endpoint "dispute" UseParam
                          .\/ Endpoint "unblock" UseParam
                          .\/ Endpoint "close" UseParam

type CollectPaymentSchema = Endpoint "collect" UseParam

startEscrowEndpoint :: Contract (Last ThreadToken) StartAppliedEscrowSchema Text ()
startEscrowEndpoint = forever
                        $ handleError logError
                        $ awaitPromise
                        $ endpoint @"publish" (\x -> publish x)

useEscrowEndpoints :: Contract (Last ThreadToken) UseAppliedEscrowSchema Text ()
useEscrowEndpoints = forever
                        $ handleError logError
                        $ awaitPromise
                        $ accept' `select` dispute' `select` unblock' `select` close'
                        where
                          accept'   = endpoint @"accept" (\x -> accept x)
                          dispute'  = endpoint @"dispute" (\x -> dispute x)
                          unblock'  = endpoint @"unblock" (\x -> unblock x)
                          close'    = endpoint @"close" (\x -> close x)

collectEscrowEndpoint :: Contract (Last ThreadToken) CollectPaymentSchema Text ()
collectEscrowEndpoint = forever
                        $ handleError logError
                        $ awaitPromise
                        $ endpoint @"collect" (\x -> collect x)


