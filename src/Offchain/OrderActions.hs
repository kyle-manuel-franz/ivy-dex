{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE NumericUnderscores  #-}

module Offchain.OrderActions (
    PlaceOrderParams (..),
    CancelOrderParams (..),
    TakeOrderParams (..),
    orderActionEndpoints
) where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Text              (Text)
import           Data.Map               as Map
import           GHC.Generics           (Generic)
import           Ledger
import           Ledger.Ada             as Ada
import qualified Ledger.Constraints     as Constraints
import           Playground.Contract    (ToSchema)
import           Plutus.Contract
import           PlutusTx               (Data (..))
import           PlutusTx
import           PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Text.Printf            (printf)

import           Contracts.Order

data PlaceOrderParams = PlaceOrderParams {
    pOwner     :: !PubKeyHash,
    pBook      :: !PubKeyHash,
    pBuyValue  :: !Value,
    pSellValue :: !Value
} deriving (Generic, ToJSON, FromJSON, ToSchema)

data CancelOrderParams = CancelOrderParams {
    cOwner     :: !PubKeyHash
} deriving (Generic, ToJSON, FromJSON, ToSchema)

data TakeOrderParams = TakeOrderParams {
    tOwner     :: !PubKeyHash,
    tBuyValue  :: !Value,
    tSellValue :: !Value
} deriving (Generic, ToJSON, FromJSON, ToSchema)

type OrderActionSchema =
            Endpoint "placeOrder"  PlaceOrderParams
        .\/ Endpoint "cancelOrder" CancelOrderParams
        .\/ Endpoint "takeOrder"   TakeOrderParams

placeOrder :: AsContractError e => PlaceOrderParams -> Contract w s e ()
placeOrder op = do
        let dat = OrderDatum {
                odOwner = pOwner op,
                odBook = Ledger.PubKeyHash "c2ff616e11299d9094ce0a7eb5b7284b705147a822f4ffbd471f971a",
                odBuyValue = pBuyValue op,
                odSellValue = pSellValue op
            }
        let p = OrderParams { scriptVersion = "0.0.1" }
            tx = Constraints.mustPayToTheScript dat $ (odSellValue dat)
        ledgerTx <- submitTxConstraints ( typedValidator p ) tx
        void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
        logInfo @String $ printf "place order endpoint"

cancelOrder :: AsContractError e => CancelOrderParams -> Contract w s e ()
cancelOrder co = do
    logInfo @String $ printf "cancel order endpoint"

takeOrder :: AsContractError e => TakeOrderParams -> Contract w s e ()
takeOrder to = do
        let bookAddress = Ledger.PubKeyHash "c2ff616e11299d9094ce0a7eb5b7284b705147a822f4ffbd471f971a"
        let p = OrderParams { scriptVersion = "0.0.1" }
        utxos <- Map.filter isSuitableUtxo <$> (utxosAt $ scrAddress p)
        logInfo @String $ printf "take order endpoint"
    where
        isSuitableUtxo :: ChainIndexTxOut -> Bool
        isSuitableUtxo o = case _ciTxOutDatum o of
            Left _ -> False
            Right (Datum e) -> case PlutusTx.fromBuiltinData e :: Maybe OrderDatum of
                Nothing -> False
                Just d -> True

orderActionEndpoints :: Contract () OrderActionSchema Text ()
orderActionEndpoints = awaitPromise (placeOrder' `select` cancelOrder' `select` takeOrder') >> orderActionEndpoints
    where
        placeOrder'  = endpoint @"placeOrder" placeOrder
        cancelOrder' = endpoint @"cancelOrder" cancelOrder
        takeOrder'   = endpoint @"takeOrder" takeOrder