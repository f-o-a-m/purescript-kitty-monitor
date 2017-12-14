module Contracts.ERC721 where

import Prelude
import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep as G
import Data.Generic.Rep.Eq as GEq
import Data.Generic.Rep.Show as GShow
import Data.Lens ((.~))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3.Types.Types (HexString(..))
import Network.Ethereum.Web3.Types (class EtherUnit, BlockMode(..), Web3, BigNumber, _address, _topics, _fromBlock, _toBlock, defaultFilter, noPay)
import Network.Ethereum.Web3.Provider (class IsAsyncProvider)
import Network.Ethereum.Web3.Contract (class EventFilter, call, sendTx)
import Network.Ethereum.Web3.Solidity
--------------------------------------------------------------------------------
-- | Eth_supportsInterfaceFn
--------------------------------------------------------------------------------

type Eth_supportsInterfaceFn = Tagged (SProxy "supportsInterface(bytes4)") (Tuple1 (BytesN D4))

eth_supportsInterface :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> (BytesN D4) -> Web3 p e Boolean
eth_supportsInterface x0 x1 cm x3 = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple1 x3) :: Eth_supportsInterfaceFn)

--------------------------------------------------------------------------------
-- | Eth_approveFn
--------------------------------------------------------------------------------

type Eth_approveFn = Tagged (SProxy "approve(address,uint256)") (Tuple2 Address (UIntN (D2 :& D5 :& D6)))

eth_approve :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> Address -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e HexString
eth_approve x0 x1 x2 x3 = sendTx x0 x1 noPay ((tagged $ Tuple2 x2 x3) :: Eth_approveFn)

--------------------------------------------------------------------------------
-- | Eth_totalSupplyFn
--------------------------------------------------------------------------------

type Eth_totalSupplyFn = Tagged (SProxy "totalSupply()") (Tuple0 )

eth_totalSupply :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> Web3 p e (UIntN (D2 :& D5 :& D6))
eth_totalSupply x0 x1 cm = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple0 ) :: Eth_totalSupplyFn)

--------------------------------------------------------------------------------
-- | Eth_transferFromFn
--------------------------------------------------------------------------------

type Eth_transferFromFn = Tagged (SProxy "transferFrom(address,address,uint256)") (Tuple3 Address Address (UIntN (D2 :& D5 :& D6)))

eth_transferFrom :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> Address -> Address -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e HexString
eth_transferFrom x0 x1 x2 x3 x4 = sendTx x0 x1 noPay ((tagged $ Tuple3 x2 x3 x4) :: Eth_transferFromFn)

--------------------------------------------------------------------------------
-- | Eth_ownerOfFn
--------------------------------------------------------------------------------

type Eth_ownerOfFn = Tagged (SProxy "ownerOf(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

eth_ownerOf :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e Address
eth_ownerOf x0 x1 cm x3 = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple1 x3) :: Eth_ownerOfFn)

--------------------------------------------------------------------------------
-- | Eth_balanceOfFn
--------------------------------------------------------------------------------

type Eth_balanceOfFn = Tagged (SProxy "balanceOf(address)") (Tuple1 Address)

eth_balanceOf :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> Address -> Web3 p e (UIntN (D2 :& D5 :& D6))
eth_balanceOf x0 x1 cm x3 = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple1 x3) :: Eth_balanceOfFn)

--------------------------------------------------------------------------------
-- | Eth_transferFn
--------------------------------------------------------------------------------

type Eth_transferFn = Tagged (SProxy "transfer(address,uint256)") (Tuple2 Address (UIntN (D2 :& D5 :& D6)))

eth_transfer :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> Address -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e HexString
eth_transfer x0 x1 x2 x3 = sendTx x0 x1 noPay ((tagged $ Tuple2 x2 x3) :: Eth_transferFn)

--------------------------------------------------------------------------------
-- | Transfer
--------------------------------------------------------------------------------

newtype Transfer = Transfer {from :: Address,to :: Address,tokenId :: (UIntN (D2 :& D5 :& D6))}

derive instance newtypeTransfer :: Newtype Transfer _

instance eventFilterTransfer :: EventFilter Transfer where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just (HexString "ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef")]
		# _fromBlock .~ Earliest 
		# _toBlock .~ Latest

instance indexedEventTransfer :: IndexedEvent (Tuple0 ) (Tuple3 (Tagged (SProxy "from") Address) (Tagged (SProxy "to") Address) (Tagged (SProxy "tokenId") (UIntN (D2 :& D5 :& D6)))) Transfer where
  isAnonymous _ = false

derive instance genericTransfer :: G.Generic Transfer _

instance eventGenericTransferShow :: Show Transfer where
	show = GShow.genericShow

instance eventGenericTransfereq :: Eq Transfer where
	eq = GEq.genericEq

--------------------------------------------------------------------------------
-- | Approval
--------------------------------------------------------------------------------

newtype Approval = Approval {owner :: Address,approved :: Address,tokenId :: (UIntN (D2 :& D5 :& D6))}

derive instance newtypeApproval :: Newtype Approval _

instance eventFilterApproval :: EventFilter Approval where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just (HexString "8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925")]
		# _fromBlock .~ Earliest 
		# _toBlock .~ Latest

instance indexedEventApproval :: IndexedEvent (Tuple0 ) (Tuple3 (Tagged (SProxy "owner") Address) (Tagged (SProxy "approved") Address) (Tagged (SProxy "tokenId") (UIntN (D2 :& D5 :& D6)))) Approval where
  isAnonymous _ = false

derive instance genericApproval :: G.Generic Approval _

instance eventGenericApprovalShow :: Show Approval where
	show = GShow.genericShow

instance eventGenericApprovaleq :: Eq Approval where
	eq = GEq.genericEq
