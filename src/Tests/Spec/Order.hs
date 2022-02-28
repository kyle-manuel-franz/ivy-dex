{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Tests.Spec.Order where

import           Offchain.OrderActions
import           Control.Monad.Freer.Extras as Extras
import           Plutus.Trace
import           Wallet.Emulator.Wallet
import           Test.Tasty
import           Plutus.Contract.Test
import           Prelude
import           Control.Monad         (void)
import           Ledger
import qualified Ledger.Ada            as Ada
import           Plutus.Contract       (Contract, ContractError(WalletError))
import           Wallet.API            (WalletAPIError(ValidationError))
import           Plutus.Contract.Test
import           Plutus.Trace.Emulator (ContractInstanceTag)
import qualified Plutus.Trace.Emulator as Trace
import qualified PlutusTx
import           Test.Tasty
import qualified Test.Tasty.HUnit      as HUnit

w1 :: Wallet
w1 = knownWallet 1

tests :: TestTree
tests = testGroup "order"
    [ checkPredicate "Expose lock endpoint"
      assertNoFailedTransactions
      myTrace
    ]

test :: IO ()
test = runEmulatorTraceIO myTrace

myTrace :: EmulatorTrace ()
myTrace = do
    let op = PlaceOrderParams {
        pOwner = Ledger.PubKeyHash "c2ff616e11299d9094ce0a7eb5b7284b705147a822f4ffbd471f971a",
        pBuyValue = Ada.lovelaceValueOf 1000000,
        pSellValue = Ada.lovelaceValueOf 1000000
    }

    h1 <- activateContractWallet (knownWallet 1) orderActionEndpoints
    callEndpoint @"placeOrder" h1 $ op
    void $ Trace.waitNSlots 2