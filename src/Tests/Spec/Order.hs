{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Tests.Spec.Order where

import Offchain.OrderActions
import Control.Monad.Freer.Extras as Extras
import Plutus.Trace
import Wallet.Emulator.Wallet
import Test.Tasty
import           Plutus.Contract.Test
import Prelude
import           Control.Monad         (void)
import           Ledger                (ValidationError(ScriptFailure))
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
    h1 <- activateContractWallet (knownWallet 1) orderActionEndpoints
    void $ Trace.waitNSlots 2