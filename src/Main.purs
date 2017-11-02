module Main where

import Prelude

import Contracts.EthereumWhite as EW
import Control.Monad.Aff (runAff_)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Reader (ReaderT)
import Data.Either (either)
import Network.Ethereum.Web3.Contract (EventAction(..), event)
import Network.Ethereum.Web3.Provider (runWeb3)
import Network.Ethereum.Web3.Types (ETH, Web3(..), Address(..), HexString(..))
import Utils (HttpProvider)


-- main :: forall e. Eff ( console :: CONSOLE
--                       , eth :: ETH 
--                       | e
--                       ) Unit
main = do

  let contractAddress = Address <<< HexString $ "39e505e1518813ab3834d57d06c22b2e5a7fb9f2"

  log "Hello EthereumWhite"
  
  runAff_ (\e -> log $ either show (\_ -> "i'm back") e) $ liftAff $ runWeb3 $
    event contractAddress $ \(EW.Transfer _from _to _value) -> do
      liftEff $ logShow $ "Event: " <> show _from <> " -> " <> show _to <> ": " <> show _value
      pure TerminateEvent :: ReaderT _ (Web3 HttpProvider _) _
