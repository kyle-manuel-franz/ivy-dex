{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Tests.Spec.Order where

import           Offchain.OrderActions
import           Control.Monad.Freer.Extras     as Extras
import           Plutus.Trace
import           Test.Tasty
import           Plutus.Contract
import           Prelude
import           Control.Monad                  (void)
import           Ledger
import qualified Ledger.Ada                     as Ada
import           Plutus.Contract               (Contract, ContractError(WalletError))
import           Wallet.API                    (WalletAPIError(ValidationError))
import           Plutus.Contract.Test
import           Plutus.Trace.Emulator         (ContractInstanceTag)
import qualified Plutus.Trace.Emulator         as Trace
import           Test.Tasty
import qualified Test.Tasty.HUnit              as HUnit

bookWallet :: Wallet
bookWallet = knownWallet 9

-- TODO: Set the config fee structure

tests :: TestTree
tests = testGroup "order"
    [ checkPredicate "Can place an order"
      (
           assertNoFailedTransactions
      .&&. walletFundsChange (knownWallet 1)(Ada.lovelaceValueOf (-1000000))
      )
      simpleOrderPlacementTrace,
      checkPredicate "Can place and take order"
      (
           assertNoFailedTransactions
        .&&. walletFundsChange (knownWallet 1)(Ada.lovelaceValueOf (10000))
        .&&. walletFundsChange bookWallet (Ada.lovelaceValueOf (1000000))
      )
      simpleOrderPlacementAndTakeTrace
    ]

test :: IO ()
test = runEmulatorTraceIO simpleOrderPlacementAndTakeTrace

cancelOrderTrace :: EmulatorTrace ()
cancelOrderTrace = do
    let op = PlaceOrderParams {
        pOwner = walletPubKeyHash (knownWallet 1),
        pBook  = walletPubKeyHash bookWallet,
        pBuyValue = Ada.lovelaceValueOf 1000000,
        pSellValue = Ada.lovelaceValueOf 1000000
    }

    h1 <- activateContractWallet (knownWallet 1) orderActionEndpoints

    callEndpoint @"placeOrder" h1 $ op
    void $ Trace.waitNSlots 2

    let co = CancelOrderParams {
        cOwner = walletPubKeyHash (knownWallet 1)
    }

    callEndpoint @"cancelOrder" h1 $ co

    void $ Trace.waitNSlots 2


simpleOrderPlacementTrace :: EmulatorTrace ()
simpleOrderPlacementTrace = do
    let op = PlaceOrderParams {
        pOwner = walletPubKeyHash (knownWallet 1),
        pBook  = walletPubKeyHash bookWallet,
        pBuyValue = Ada.lovelaceValueOf 1000000,
        pSellValue = Ada.lovelaceValueOf 1000000
    }

    h1 <- activateContractWallet (knownWallet 1) orderActionEndpoints

    callEndpoint @"placeOrder" h1 $ op
    void $ Trace.waitNSlots 2


simpleOrderPlacementAndTakeTrace :: EmulatorTrace ()
simpleOrderPlacementAndTakeTrace = do
    let op = PlaceOrderParams {
        pOwner = walletPubKeyHash (knownWallet 1),
        pBook  = walletPubKeyHash bookWallet,
        pBuyValue = Ada.lovelaceValueOf 1000000,
        pSellValue = Ada.lovelaceValueOf 1000000
    }

    h1 <- activateContractWallet (knownWallet 1) orderActionEndpoints
    h2 <- activateContractWallet (knownWallet 2) orderActionEndpoints

    callEndpoint @"placeOrder" h1 $ op
    void $ Trace.waitNSlots 2

    callEndpoint @"takeOrder" h2 $ TakeOrderParams {
        tOwner = walletPubKeyHash (knownWallet 1),
        tBook  = walletPubKeyHash bookWallet,
        tBuyValue = Ada.lovelaceValueOf 1000000,
        tSellValue = Ada.lovelaceValueOf 1000000
    }

    void $ Trace.waitNSlots 2