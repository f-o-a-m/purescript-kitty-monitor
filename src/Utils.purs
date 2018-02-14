module Utils (MyProvider, myProvider) where

import Prelude

import Control.Monad.Aff (liftEff')
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Exception (throw)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Data.Maybe (maybe)
import Network.Ethereum.Web3.Provider (class IsAsyncProvider, httpProvider)
import Node.Process (lookupEnv)
import Type.Proxy (Proxy(..))

data MyProvider

instance providerHttp :: IsAsyncProvider MyProvider where
  getAsyncProvider = liftAff <<< liftEff' $ do
    murl <- unsafeCoerceEff $ lookupEnv "NODE_URL"
    url <- maybe (throw "No NODE_URL env.") pure murl
    httpProvider url

myProvider :: Proxy MyProvider
myProvider = Proxy
