module Test.Trace where

import Control.Monad.Freer.Extras as Extras
import Data.Default               (Default (..))
import Data.Functor               (void)
import Ledger.TimeSlot
import Plutus.Trace
import Wallet.Emulator.Wallet

import Token.OffChain2

-- Contract w s e a
-- EmulatorTrace a

-- test :: IO ()
-- test = runEmulatorTraceIO myTrace

-- myTrace :: EmulatorTrace ()
-- myTrace = do
--     h1 <- activateContractWallet (knownWallet 1) endpoints
--     h2 <- activateContractWallet (knownWallet 2) endpoints
--     callEndpoint @"mint" h1 $ TokenName "aaa"
--     void $ waitUntilSlot 10
--     -- callEndpoint @"grab" h2 ()
--     -- s <- waitNSlots 2
--     -- Extras.logInfo $ "reached " ++ show s