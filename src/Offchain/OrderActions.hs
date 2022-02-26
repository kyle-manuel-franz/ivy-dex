{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE NumericUnderscores    #-}

module Offchain.OrderActions where

import           Ledger
import           Plutus.Contract
import           Prelude
import           Text.Printf         (printf)

data PlaceOrderParams = PlaceOrderParams {
    pOwner     :: !PubKeyHash,
    pBuyValue  :: !Value,
    pSellValue :: !Value
}

data CancelOrderParams = CancelOrderParams {
    cOwner     :: !PubKeyHash
}

data TakeOrderParams = TakeOrderParams {
    tOwner     :: !PubKeyHash,
    tBuyValue  :: !Value,
    tSellValue :: !Value
}

type OrderActionSchema =
            Endpoint "placeOrder"  PlaceOrderParams
        .\/ Endpoint "cancelOrder" CancelOrderParams
        .\/ Endpoint "takeOrder"   TakeOrderParams


placeOrder :: AsContractError e => PlaceOrderParams -> Contract w s e ()
placeOrder op = do
    logInfo @String $ printf "place order endpoint"

cancelOrder :: AsContractError e => CancelOrderParams -> Contract w s e ()
cancelOrder co = do
    logInfo @String $ printf "cancel order endpoint"

takeOrder :: AsContractError e => TakeOrderParams -> Contract w s e ()
takeOrder to = do
    logInfo @String $ printf "take order endpoint"






















