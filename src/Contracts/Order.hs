{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RecordWildCards            #-}

module Contracts.Order where

import           Ledger               hiding (singleton)
import           Ledger.Value         as Value
import qualified Ledger.Typed.Scripts      as Scripts
import qualified PlutusTx
import           PlutusTx.Prelude

data OrderDatum = OrderDatum
    {
        odOwner      :: PubKeyHash,
        odBook       :: PubKeyHash,
        odBuyValue :: Value,
        odSellValue  :: Value
    }

PlutusTx.unstableMakeIsData ''OrderDatum

data OrderRedeemer = Take | Cancel
PlutusTx.unstableMakeIsData ''OrderRedeemer

data OrderParams = OrderParams {
    scriptVersion :: BuiltinByteString
}
PlutusTx.makeLift ''OrderParams

{-# INLINABLE mkValidator #-}
mkValidator :: OrderParams -> OrderDatum -> OrderRedeemer -> ScriptContext -> Bool
mkValidator OrderParams {..} dat r ctx =
                    case r of
                        Take ->
                            traceIfFalse "Fee collector script must get paid" feesPaidToBook &&
                            traceIfFalse "Owner must get the tokens and fees" ownerMustGetTokensAndFees &&
                            traceIfFalse "Signing key must get lovelace" signerMustRedeemValue
                        Cancel ->
                            traceIfFalse "Only owner can cancel order" signedByOwner

    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        signedByOwner :: Bool
        signedByOwner = txSignedBy info $ odOwner dat

        -- TODO: implement this
        feesPaidToBook :: Bool
        feesPaidToBook = True

        -- TODO: implement this
        ownerMustGetTokensAndFees :: Bool
        ownerMustGetTokensAndFees = True

        -- TODO: implement this
        signerMustRedeemValue :: Bool
        signerMustRedeemValue = True

data Order
instance Scripts.ValidatorTypes Order where
    type instance DatumType Order = OrderDatum
    type instance RedeemerType Order = OrderRedeemer

typedValidator :: OrderParams -> Scripts.TypedValidator Order
typedValidator p = Scripts.mkTypedValidator @Order
        ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
        $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @OrderDatum @OrderRedeemer