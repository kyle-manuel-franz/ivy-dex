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


