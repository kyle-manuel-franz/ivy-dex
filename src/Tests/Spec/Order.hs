{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module Tests.Spec.Order where

import           Offchain.OrderActions
import           Control.Monad.Freer.Extras     as Extras
import           Control.Lens
import           Data.Default                   (Default (..))
import qualified Data.Map                       as Map
import           Plutus.Trace
import           Test.Tasty
import           Plutus.Contract
import           Prelude                        (IO, String, Show (..))
import           Control.Monad                  (void)
import           Ledger
import           Ledger.Fee
import qualified Ledger.Ada                     as Ada
import           Ledger.Value                   as Value
import           Plutus.Contract               (Contract, ContractError(WalletError))
import           Wallet.API                    (WalletAPIError(ValidationError))
import           Plutus.Contract.Test
import           Plutus.Trace.Emulator         (ContractInstanceTag)
import qualified Plutus.Trace.Emulator         as Trace
import           PlutusTx.Prelude
import           Test.Tasty
import qualified Test.Tasty.HUnit              as HUnit

bookWallet :: Wallet
bookWallet = knownWallet 9

currency :: CurrencySymbol
currency = "aa"

name :: TokenName
name = "A"

token :: AssetClass
token = AssetClass (currency, name)

av :: Value
av = Value.singleton currency name 10_000

fc :: FeeConfig
fc = FeeConfig { fcConstantFee = Ada.lovelaceOf 1_000, fcScriptsFeeFactor = 0.0 }

emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [(knownWallet w, v) | w <- [1 .. 10]]) def fc
    where
        v :: Value
        v = Ada.lovelaceValueOf 1_000_000_000 <> assetClassValue token 1_000_000

tests :: TestTree
tests = testGroup "order"
    [ checkPredicateOptions (defaultCheckOptions & emulatorConfig .~ emCfg) "Can place an order"
      (
              assertNoFailedTransactions
         .&&. walletFundsChange (knownWallet 1)(Ada.lovelaceValueOf (-1000000))
      )
      simpleOrderPlacementTrace,
      checkPredicateOptions (defaultCheckOptions & emulatorConfig .~ emCfg)  "Can place and take order"
      (
             assertNoFailedTransactions
        .&&. walletFundsChange (knownWallet 1)(av <> (Ada.lovelaceValueOf (-1000000)))
        .&&. walletFundsChange bookWallet (Ada.lovelaceValueOf (10000))
      )
      simpleOrderPlacementAndTakeTrace,
      checkPredicateOptions (defaultCheckOptions & emulatorConfig .~ emCfg)  "Can place and not take order if wrong value paid"
      (
         assertFailedTransaction (\
         _ err _ -> case err of
             {
                 ScriptFailure (EvaluationError ["Owner must get the tokens and fees", "PT5"] _) -> True;
                 _ -> False
         })
      )
      ownerMustGetPaidCorrectTokenTrace,
      checkPredicate "Can place and cancel order"
      (
              assertNoFailedTransactions
         .&&. walletFundsChange (knownWallet 1) (Ada.lovelaceValueOf 0)
      )
      cancelOrderTrace,
      checkPredicate "Can place and non owner cannot cancel order"
      (
         assertFailedTransaction (\
            _ err _ -> case err of
                {
                    ScriptFailure (EvaluationError ["Only owner can cancel order", "PT5"] _) -> True;
                    _ -> False
            })
      )
      cancelOrderNonOwnerTrace
    ]

test :: IO ()
test = runEmulatorTraceIO' def emCfg simpleOrderPlacementAndTakeTrace

cancelOrderTrace :: EmulatorTrace ()
cancelOrderTrace = do
    let op = PlaceOrderParams {
        pOwner = walletPubKeyHash (knownWallet 1),
        pBook  = walletPubKeyHash bookWallet,

        pBuyTokenName       = Ada.adaToken,
        pBuyCurrencySymbol  = Ada.adaSymbol,
        pBuyTokenAmount     = 1000000,

        pSellTokenName      = Ada.adaToken,
        pSellCurrencySymbol = Ada.adaSymbol,
        pSellTokenAmount    = 1000000
    }

    h1 <- activateContractWallet (knownWallet 1) orderActionEndpoints

    callEndpoint @"placeOrder" h1 $ op
    void $ Trace.waitNSlots 2

    let co = CancelOrderParams {
        cOwner = walletPubKeyHash (knownWallet 1)
    }

    callEndpoint @"cancelOrder" h1 $ co
    void $ Trace.waitNSlots 2

cancelOrderNonOwnerTrace :: EmulatorTrace ()
cancelOrderNonOwnerTrace = do
    let op = PlaceOrderParams {
        pOwner = walletPubKeyHash (knownWallet 1),
        pBook  = walletPubKeyHash bookWallet,

        pBuyTokenName       = Ada.adaToken,
        pBuyCurrencySymbol  = Ada.adaSymbol,
        pBuyTokenAmount     = 1000000,

        pSellTokenName      = Ada.adaToken,
        pSellCurrencySymbol = Ada.adaSymbol,
        pSellTokenAmount    = 1000000
    }

    h1 <- activateContractWallet (knownWallet 1) orderActionEndpoints
    h2 <- activateContractWallet (knownWallet 2) orderActionEndpoints

    callEndpoint @"placeOrder" h1 $ op
    void $ Trace.waitNSlots 2

    let co = CancelOrderParams {
        cOwner = walletPubKeyHash (knownWallet 1)
    }

    callEndpoint @"cancelOrder" h2 $ co
    void $ Trace.waitNSlots 2

simpleOrderPlacementTrace :: EmulatorTrace ()
simpleOrderPlacementTrace = do
    let op = PlaceOrderParams {
        pOwner = walletPubKeyHash (knownWallet 1),
        pBook  = walletPubKeyHash bookWallet,

        pBuyTokenName       = Ada.adaToken,
        pBuyCurrencySymbol  = Ada.adaSymbol,
        pBuyTokenAmount     = 1000000,

        pSellTokenName      = name,
        pSellCurrencySymbol = currency,
        pSellTokenAmount    = 10_000
    }

    h1 <- activateContractWallet (knownWallet 1) orderActionEndpoints

    callEndpoint @"placeOrder" h1 $ op
    void $ Trace.waitNSlots 2

simpleOrderPlacementAndTakeTrace :: EmulatorTrace ()
simpleOrderPlacementAndTakeTrace = do
    let op = PlaceOrderParams {
        pOwner = walletPubKeyHash (knownWallet 1),
        pBook  = walletPubKeyHash bookWallet,

        pBuyTokenName       = Ada.adaToken,
        pBuyCurrencySymbol  = Ada.adaSymbol,
        pBuyTokenAmount     = 1000000,

        pSellTokenName      = name,
        pSellCurrencySymbol = currency,
        pSellTokenAmount    = 10_000
    }

    h1 <- activateContractWallet (knownWallet 1) orderActionEndpoints
    h2 <- activateContractWallet (knownWallet 2) orderActionEndpoints

    callEndpoint @"placeOrder" h1 $ op
    void $ Trace.waitNSlots 2

    callEndpoint @"takeOrder" h2 $ TakeOrderParams {
        tOwner = walletPubKeyHash (knownWallet 1),
        tBook  = walletPubKeyHash bookWallet,
        tBuyValue = Ada.lovelaceValueOf 100000,
        tSellValue = av
    }

    void $ Trace.waitNSlots 2

ownerMustGetPaidCorrectTokenTrace :: EmulatorTrace ()
ownerMustGetPaidCorrectTokenTrace = do
    let op = PlaceOrderParams {
        pOwner = walletPubKeyHash (knownWallet 1),
        pBook  = walletPubKeyHash bookWallet,

        pBuyTokenName       = Ada.adaToken,
        pBuyCurrencySymbol  = Ada.adaSymbol,
        pBuyTokenAmount     = 1000000,

        pSellTokenName      = name,
        pSellCurrencySymbol = currency,
        pSellTokenAmount    = 10_000
    }

    h1 <- activateContractWallet (knownWallet 1) orderActionEndpoints
    h2 <- activateContractWallet (knownWallet 2) orderActionEndpoints

    callEndpoint @"placeOrder" h1 $ op
    void $ Trace.waitNSlots 2

    let wrongVal = Value.singleton currency name 1_000

    callEndpoint @"takeOrder" h2 $ TakeOrderParams {
        tOwner = walletPubKeyHash (knownWallet 1),
        tBook  = walletPubKeyHash bookWallet,
        tBuyValue = Ada.lovelaceValueOf 100000,
        tSellValue = wrongVal
    }

    void $ Trace.waitNSlots 2

-- TODO: Test Double Spend
doubleSpendTrace :: EmulatorTrace ()
doubleSpendTrace = do

    void $ Trace.waitNSlots 2