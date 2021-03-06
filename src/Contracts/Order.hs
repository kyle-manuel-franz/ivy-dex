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
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE NumericUnderscores #-}

module Contracts.Order where

import           Ledger.Ada                as Ada
import           Ledger                    hiding (singleton)
import           Ledger.Value              as Value
import qualified Ledger.Typed.Scripts      as Scripts
import qualified PlutusTx
import           PlutusTx.Prelude

data OrderDatum = OrderDatum
    {
        odOwner                :: !PubKeyHash,
        odBook                 :: !PubKeyHash,

        odBuyerTokenName       :: !TokenName,
        odBuyerCurrencySymbol  :: !CurrencySymbol,
        odBuyerTokenAmount     :: !Integer,

        odSellerTokenName      :: !TokenName,
        odSellerCurrencySymbol :: !CurrencySymbol,
        odSellerTokenAmount    :: !Integer
    }
PlutusTx.unstableMakeIsData ''OrderDatum
PlutusTx.makeLift ''OrderDatum

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
        minRequiredFeeToBook :: Integer
        minRequiredFeeToBook = 10000

        ownerMakerFeeLovelace :: Integer
        ownerMakerFeeLovelace = 1000000

        info :: TxInfo
        info = scriptContextTxInfo ctx

        txFees :: Value
        txFees = txInfoFee info

        ownInput :: TxInInfo
        ownInput = case findOwnInput ctx of
            Just i -> i
            Nothing -> traceError "expected input from script"

        signedByOwner :: Bool
        signedByOwner = txSignedBy info $ odOwner dat

        valueToOwner :: Value
        valueToOwner = valuePaidTo info $ odOwner dat

        ownInputOutput :: TxOut
        ownInputOutput = txInInfoResolved ownInput

        ownInputValue :: Value
        ownInputValue = txOutValue ownInputOutput

        txOutputs :: [TxOut]
        txOutputs = txInfoOutputs info

        txSignatories :: [PubKeyHash]
        txSignatories = txInfoSignatories info

        sellerPubKeyHash :: PubKeyHash
        sellerPubKeyHash =
            case txSignatories of
                [s] -> s
                _ -> traceError "expected only one signatory per transaction"

        sellerOutputs :: [TxOut]
        sellerOutputs = [o | o <- txOutputs, txOutAddress o == pubKeyHashAddress sellerPubKeyHash]

        sellerTotalValues :: [Value]
        sellerTotalValues = fmap txOutValue sellerOutputs

        sellerTotalValue :: Value
        sellerTotalValue = mconcat sellerTotalValues

        -- need to make sure they get the maker fee in addition to this fee
        feesPaidToBook :: Bool
        feesPaidToBook = Ada.fromValue (valuePaidTo info $ odBook dat) == Ada.lovelaceOf minRequiredFeeToBook

        sellerValue :: Value
        sellerValue = Value.singleton (odSellerCurrencySymbol dat) (odSellerTokenName dat) (odSellerTokenAmount dat)

        ownerMustGetTokensAndFees :: Bool
        ownerMustGetTokensAndFees = valueToOwner == sellerValue

        -- TODO: implement this
        signerMustRedeemValue :: Bool
        signerMustRedeemValue = True

        -- TODO: implement protections against double spend
        -- TODO: I think it would be best to be very strict about
        -- TODO: the number of inputs and outputs allowed

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

validator :: OrderParams -> Validator
validator = Scripts.validatorScript . typedValidator

scrAddress :: OrderParams -> Ledger.Address
scrAddress = scriptAddress . validator