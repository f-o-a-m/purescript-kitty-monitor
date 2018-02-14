module App.Config where

import Prelude

import Contracts.CryptoKitties as CK
import Control.Monad.Aff (liftEff')
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Exception (throw)
import Data.Either (Either(..))
import Data.Lens ((.~))
import Data.Maybe (Maybe(..), fromJust)
import Network.Ethereum.Web3 (class IsAsyncProvider, Address, ChainCursor(..), Web3, _to, defaultTransactionOptions, mkAddress, mkHexString)
import Partial.Unsafe (unsafePartial)

ckAddress :: Address
ckAddress = unsafePartial fromJust $
            mkAddress =<< mkHexString "c7af99fe5513eb6710e6d5f44f9989da40f27f26"


tokenContract :: forall eff p . IsAsyncProvider p => Web3 p eff Address
tokenContract = do
  let txOpts = defaultTransactionOptions # _to .~ Just ckAddress
  eAddr <- CK.eth_nonFungibleContract txOpts Latest
  case eAddr of
    Left _ -> liftAff <<< liftEff' $ throw "No Token Contract found!"
    Right addr -> pure addr
