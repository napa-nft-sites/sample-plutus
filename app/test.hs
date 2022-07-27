import           Week06.Token.OnChain
import           Week06.Utils                (getCredentials)
import           Plutus.Contract             as Contract
import           Plutus.Contract.Wallet      (getUnspentOutput)
import           Control.Monad               hiding (fmap)
import           Prelude                     (Semigroup (..), Show (..), String)

main :: IO
main = do
  [addr] <- getArgs
  case getCredentials addr of
    Nothing      -> Contract.throwError $ pack $ printf "expected pubkey address, but got %s" $ show addr
    Just (x, my) -> do
        oref <- getUnspentOutput
        printf (show oref)
        -- o    <- fromJust <$> Contract.txOutFromRef oref 