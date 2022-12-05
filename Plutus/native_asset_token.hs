{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

<<<<<<< HEAD
module Week05.Test where
=======
>>>>>>> dc0f108781b857620e791b0b15b23f5a19de4a22

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           PlutusTx.Builtins
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Ledger.Address
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet


{- on-chain code -}

{-# INLINABLE mkPolicy #-}
<<<<<<< HEAD
mkPolicy :: PaymentPubKeyHash -> () -> ScriptContext -> Bool
mkPolicy pkh' () ctx = if isActionMinting then
=======
mkPolicy :: PaymentPubKeyHash -> TokenName ->() -> ScriptContext -> Bool
mkPolicy pkh' tn () ctx = if isActionMinting then
>>>>>>> dc0f108781b857620e791b0b15b23f5a19de4a22
                          isValidPubKeyHash
                          else True 
                
    where

        info :: TxInfo
        info = scriptContextTxInfo ctx


        isActionMinting :: Bool
        isActionMinting  = case flattenValue (txInfoMint info) of
<<<<<<< HEAD
            [(_, _, amt)]   -> amt > 0
=======
            [(_, tn, amt)]   -> amt > 0
>>>>>>> dc0f108781b857620e791b0b15b23f5a19de4a22
       
        isValidPubKeyHash :: Bool
        isValidPubKeyHash = txSignedBy (scriptContextTxInfo ctx) $ unPaymentPubKeyHash pkh'


{- off-chain code -} 

<<<<<<< HEAD
policy :: PaymentPubKeyHash ->  Scripts.MintingPolicy
policy pkh = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh

curSymbol ::  PaymentPubKeyHash -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy
=======
policy :: PaymentPubKeyHash -> TokenName ->  Scripts.MintingPolicy
policy pkh tn = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \pkh' tn' -> Scripts.wrapMintingPolicy $ mkPolicy pkh' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn

curSymbol ::  PaymentPubKeyHash -> TokenName -> CurrencySymbol
curSymbol pkh tn = scriptCurrencySymbol $ policy pkh tn
>>>>>>> dc0f108781b857620e791b0b15b23f5a19de4a22

data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type FreeSchema = Endpoint "mint" MintParams


pkh :: PaymentPubKeyHash
pkh = PaymentPubKeyHash ("80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7")

mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do
    pkh' <- Contract.ownPaymentPubKeyHash
<<<<<<< HEAD
    let val     = Value.singleton (curSymbol pkh) (mpTokenName mp) (mpAmount mp)
        lookups = Constraints.mintingPolicy $ policy pkh
=======
    let val     = Value.singleton (curSymbol pkh (mpTokenName mp)) (mpTokenName mp) (mpAmount mp)
        lookups = Constraints.mintingPolicy $ policy pkh (mpTokenName mp)
>>>>>>> dc0f108781b857620e791b0b15b23f5a19de4a22
        tx      = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () FreeSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

mkSchemaDefinitions ''FreeSchema

mkKnownCurrencies []


test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = "ABC"
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 555
        }
    callEndpoint @"mint" h2 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 444
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h2 $ MintParams
        { mpTokenName = tn
        , mpAmount    = -222
        }
    void $ Emulator.waitNSlots 1
