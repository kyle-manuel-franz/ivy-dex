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

-- TODO: Put failure tests cases here to assert that these transactions fail