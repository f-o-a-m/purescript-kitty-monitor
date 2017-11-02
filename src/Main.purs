module Main where

import Prelude

import Contracts.EthereumWhite as EW
import Contracts.DecentrEx as DX
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

  let ewAddress = Address <<< HexString $ "39e505e1518813ab3834d57d06c22b2e5a7fb9f2"
  let dxAddress = Address <<< HexString $ "bf29685856fae1e228878dfb35b280c0adcc3b05"

  log $ "Hello EthereumWhite at " <> show ewAddress
  
  runAff_ (\e -> log $ either show (\_ -> "i'm back (EW)") e) $ liftAff $ runWeb3 $
    -- Transfer(address indexed _from, address indexed _to, uint256 _value);
    event ewAddress $ \(EW.Transfer _from _to _value) -> do
      liftEff $ logShow $ "EW.Transfer: " 
                       <> show _from 
                       <> " -> " 
                       <> show _to <> ": " <> show _value
      pure TerminateEvent :: ReaderT _ (Web3 HttpProvider _) _

  log $ "Hello DecentrEx at " <> show dxAddress

  runAff_ (\e -> log $ either show (\_ -> "i'm back (DX)") e) $ liftAff $ runWeb3 $
    --  Trade(address tokenGet, uint amountGet, address tokenGive, uint amountGive, address get, address give);
    event dxAddress $ \(DX.Trade _tokenGet _amountGet _tokenGive _amountGive _get _give) -> do
      liftEff $ logShow $ "DX.Trade: " 
                       <> show _tokenGet <> " : " <> show _amountGet
                       <> " -> " 
                       <> show _tokenGive <> " : " <> show _amountGive
                       <> " :: "
                       <> show _get <> " -> " <> show _give
      pure TerminateEvent :: ReaderT _ (Web3 HttpProvider _) _
