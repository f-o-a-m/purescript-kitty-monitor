module App.Config where

import Prelude

import Contracts.CryptoKitties as CK
import Data.Maybe (Maybe(..), fromJust)
import Network.Ethereum.Web3 (class IsAsyncProvider, Address, ChainCursor(..), Web3, mkAddress, mkHexString)
import Partial.Unsafe (unsafePartial)

ckAddress :: Address
ckAddress = unsafePartial fromJust $
            mkAddress =<< mkHexString "c7af99fe5513eb6710e6d5f44f9989da40f27f26"


tokenContract :: forall eff p . IsAsyncProvider p => Web3 p eff Address
tokenContract = CK.eth_nonFungibleContract ckAddress Nothing Latest
