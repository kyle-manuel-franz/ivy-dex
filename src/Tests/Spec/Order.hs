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

tests :: TestTree
tests = testGroup "order"
    [ checkPredicate "Expose lock endpoint"
      (assertNoFailedTransactions
      .&&. walletFundsChange (knownWallet 1)(Ada.lovelaceValueOf (-1000000)))
      myTrace
    ]

test :: IO ()
test = runEmulatorTraceIO myTrace

myTrace :: EmulatorTrace ()
myTrace = do
    let op = PlaceOrderParams {
        pOwner = walletPubKeyHash (knownWallet 1),
        pBook  = walletPubKeyHash (knownWallet 9),
        pBuyValue = Ada.lovelaceValueOf 1000000,
        pSellValue = Ada.lovelaceValueOf 1000000
    }

    h1 <- activateContractWallet (knownWallet 1) orderActionEndpoints

    callEndpoint @"placeOrder" h1 $ op
    void $ Trace.waitNSlots 2