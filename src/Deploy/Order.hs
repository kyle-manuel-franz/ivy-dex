{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Deploy.Order where

import           Cardano.Api
import           Cardano.Api.Shelley   (PlutusScript (..))
import           Codec.Serialise       (serialise)
import           Data.Aeson            (encode)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           PlutusTx              (Data (..))
import qualified PlutusTx
import qualified Ledger
import           Ledger.Ada          as Ada

import Contracts.Order

dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript

writeOrderValidator :: Integer -> IO (Either (FileError ()) ())
writeOrderValidator v = writeValidator ("dist/testnet/order_v_"++(show v)++".plutus") $ validator $ OrderParams
    { scriptVersion = "0.0.1"
    }


writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeDatum :: IO ()
writeDatum = writeJSON "dist/testnet/datum.json" $ OrderDatum {
    odOwner = Ledger.PubKeyHash "c2ff616e11299d9094ce0a7eb5b7284b705147a822f4ffbd471f971a",
    odBook  = Ledger.PubKeyHash "c2ff616e11299d9094ce0a7eb5b7284b705147a822f4ffbd471f971a",

    odBuyerTokenName = Ada.adaToken,
    odBuyerCurrencySymbol = Ada.adaSymbol,
    odBuyerTokenAmount = 10000,

    odSellerTokenName = Ada.adaToken,
    odSellerCurrencySymbol = Ada.adaSymbol,
    odSellerTokenAmount = 10000
}