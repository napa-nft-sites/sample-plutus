{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Trace
    ( testToken
    ) where

import           Control.Monad              hiding (fmap)
import           Plutus.Contract            as Contract
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (IO)
import           Wallet.Emulator.Wallet

import           Token.OffChain2

testToken :: IO ()
testToken = runEmulatorTraceIO tokenTrace

tokenTrace :: EmulatorTrace ()
tokenTrace = do
    let w1 = knownWallet 1
        w2 = knownWallet 2
    void $ activateContractWallet w1 $ void $ mintToken @() @Empty TokenParams
        { tpToken   = "USDT"
        , tpAmount  = 100_000
        , tpAddress = mockWalletAddress w1
        }
    void $ Emulator.waitNSlots 1        

    -- void $ activateContractWallet w1 $ void $ transferToken @() @Empty TokenParams
    --     { tpToken   = "USDT"
    --     , tpAmount  = 10_000
    --     , tpAddress = mockWalletAddress w2
    --     }
    -- void $ Emulator.waitNSlots 1        

    -- void $ activateContractWallet w1 $ void $ burnToken @() @Empty TokenParams
    --     { tpToken   = "USDT"
    --     , tpAmount  = 20_000
    --     , tpAddress = mockWalletAddress w1
    --     }
    -- void $ Emulator.waitNSlots 1
