{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Token.OnChain
    ( tokenPolicy
    , tokenCurSymbol
    , burnValidator
    , burnValHash
    ) where

import qualified PlutusTx
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           Ledger                      hiding (mint, singleton)
import qualified Ledger.Typed.Scripts        as Scripts
import           Ledger.Value                as Value
import           Ledger.Scripts

{-# INLINABLE mkTokenPolicy #-}
mkTokenPolicy :: TxOutRef -> TokenName -> Integer -> () -> ScriptContext -> Bool
mkTokenPolicy oref tn amt () ctx =  traceIfFalse "UTxO not consumed"   hasUTxO           &&
                                    traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn', amt')] -> tn' == tn && amt' == amt
        _                -> False

tokenPolicy :: TxOutRef -> TokenName -> Integer -> Scripts.MintingPolicy
tokenPolicy oref tn amt = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' amt' -> Scripts.wrapMintingPolicy $ mkTokenPolicy oref' tn' amt' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
    `PlutusTx.applyCode`
    PlutusTx.liftCode amt

tokenCurSymbol :: TxOutRef -> TokenName -> Integer -> CurrencySymbol
tokenCurSymbol oref tn = scriptCurrencySymbol . tokenPolicy oref tn

{-# INLINABLE mkBurnValidator #-}
mkBurnValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkBurnValidator _ _ _ = traceError "BURNT!"

burnValidator :: Validator
burnValidator = mkValidatorScript
    $$(PlutusTx.compile [|| mkBurnValidator ||])

burnValHash :: Ledger.ValidatorHash
burnValHash = Ledger.Scripts.validatorHash burnValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress burnValidator