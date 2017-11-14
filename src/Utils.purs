module Utils (MyProvider, myProvider) where

import Prelude

import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Except (runExcept)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Prisms (_Object, _String)
import Data.Either (either)
import Data.EitherR (fmapL)
import Data.Foreign (renderForeignError)
import Data.Foreign.Class (decode, encode)
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.Maybe (maybe)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Network.Ethereum.Web3.Api (net_version)
import Network.Ethereum.Web3.Provider (class IsAsyncProvider, Provider, Provider, httpProvider, runWeb3)
import Network.Ethereum.Web3.Types (Address, ETH, Web3(..))
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (FS, readTextFile)
import Node.Process (PROCESS, lookupEnv)
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
