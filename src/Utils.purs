module Utils (MyProvider, myProvider) where

import Prelude

import Control.Monad.Aff (liftEff')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Data.Maybe (maybe)
import Network.Ethereum.Web3.Provider (class IsAsyncProvider, Provider, httpProvider)
import Network.Ethereum.Web3.Types (ETH, Web3(..))
import Node.Process (lookupEnv)
import Type.Proxy (Proxy(..))

makeProvider :: forall eff . Eff (eth :: ETH, exception :: EXCEPTION | eff) Provider
makeProvider = unsafeCoerceEff $ do
  murl <- lookupEnv "NODE_URL"
  url <- maybe (throw "Must provide NODE_URL") pure murl
  httpProvider url

data MyProvider

instance providerHttp :: IsAsyncProvider MyProvider where
  getAsyncProvider = Web3 <<< liftEff' $ makeProvider

myProvider :: Proxy MyProvider
myProvider = Proxy
