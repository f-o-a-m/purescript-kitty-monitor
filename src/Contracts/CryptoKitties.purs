module Contracts.CryptoKitties where

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
-- | Eth_createAuctionFn
--------------------------------------------------------------------------------

type Eth_createAuctionFn = Tagged (SProxy "createAuction(uint256,uint256,uint256,uint256,address)") (Tuple5 (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6)) Address)

eth_createAuction :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> (UIntN (D2 :& D5 :& D6)) -> (UIntN (D2 :& D5 :& D6)) -> (UIntN (D2 :& D5 :& D6)) -> (UIntN (D2 :& D5 :& D6)) -> Address -> Web3 p e HexString
eth_createAuction x0 x1 x2 x3 x4 x5 x6 = sendTx x0 x1 noPay ((tagged $ Tuple5 x2 x3 x4 x5 x6) :: Eth_createAuctionFn)

--------------------------------------------------------------------------------
-- | Eth_unpauseFn
--------------------------------------------------------------------------------

type Eth_unpauseFn = Tagged (SProxy "unpause()") (Tuple0 )

eth_unpause :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> Web3 p e HexString
eth_unpause x0 x1 = sendTx x0 x1 noPay ((tagged $ Tuple0 ) :: Eth_unpauseFn)

--------------------------------------------------------------------------------
-- | Eth_bidFn
--------------------------------------------------------------------------------

type Eth_bidFn = Tagged (SProxy "bid(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

eth_bid :: forall e p u . IsAsyncProvider p => EtherUnit u => Maybe Address -> Address -> u -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e HexString
eth_bid x0 x1 u x3 = sendTx x0 x1 u ((tagged $ Tuple1 x3) :: Eth_bidFn)

--------------------------------------------------------------------------------
-- | Eth_pausedFn
--------------------------------------------------------------------------------

type Eth_pausedFn = Tagged (SProxy "paused()") (Tuple0 )

eth_paused :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> Web3 p e Boolean
eth_paused x0 x1 cm = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple0 ) :: Eth_pausedFn)

--------------------------------------------------------------------------------
-- | Eth_withdrawBalanceFn
--------------------------------------------------------------------------------

type Eth_withdrawBalanceFn = Tagged (SProxy "withdrawBalance()") (Tuple0 )

eth_withdrawBalance :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> Web3 p e HexString
eth_withdrawBalance x0 x1 = sendTx x0 x1 noPay ((tagged $ Tuple0 ) :: Eth_withdrawBalanceFn)

--------------------------------------------------------------------------------
-- | Eth_isSiringClockAuctionFn
--------------------------------------------------------------------------------

type Eth_isSiringClockAuctionFn = Tagged (SProxy "isSiringClockAuction()") (Tuple0 )

eth_isSiringClockAuction :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> Web3 p e Boolean
eth_isSiringClockAuction x0 x1 cm = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple0 ) :: Eth_isSiringClockAuctionFn)

--------------------------------------------------------------------------------
-- | Eth_getAuctionFn
--------------------------------------------------------------------------------

type Eth_getAuctionFn = Tagged (SProxy "getAuction(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

eth_getAuction :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e (Tuple5 Address (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6)))
eth_getAuction x0 x1 cm x3 = call x0 x1 cm ((tagged $ Tuple1 x3) :: Eth_getAuctionFn)

--------------------------------------------------------------------------------
-- | Eth_ownerCutFn
--------------------------------------------------------------------------------

type Eth_ownerCutFn = Tagged (SProxy "ownerCut()") (Tuple0 )

eth_ownerCut :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> Web3 p e (UIntN (D2 :& D5 :& D6))
eth_ownerCut x0 x1 cm = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple0 ) :: Eth_ownerCutFn)

--------------------------------------------------------------------------------
-- | Eth_pauseFn
--------------------------------------------------------------------------------

type Eth_pauseFn = Tagged (SProxy "pause()") (Tuple0 )

eth_pause :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> Web3 p e HexString
eth_pause x0 x1 = sendTx x0 x1 noPay ((tagged $ Tuple0 ) :: Eth_pauseFn)

--------------------------------------------------------------------------------
-- | Eth_cancelAuctionWhenPausedFn
--------------------------------------------------------------------------------

type Eth_cancelAuctionWhenPausedFn = Tagged (SProxy "cancelAuctionWhenPaused(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

eth_cancelAuctionWhenPaused :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e HexString
eth_cancelAuctionWhenPaused x0 x1 x2 = sendTx x0 x1 noPay ((tagged $ Tuple1 x2) :: Eth_cancelAuctionWhenPausedFn)

--------------------------------------------------------------------------------
-- | Eth_ownerFn
--------------------------------------------------------------------------------

type Eth_ownerFn = Tagged (SProxy "owner()") (Tuple0 )

eth_owner :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> Web3 p e Address
eth_owner x0 x1 cm = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple0 ) :: Eth_ownerFn)

--------------------------------------------------------------------------------
-- | Eth_cancelAuctionFn
--------------------------------------------------------------------------------

type Eth_cancelAuctionFn = Tagged (SProxy "cancelAuction(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

eth_cancelAuction :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e HexString
eth_cancelAuction x0 x1 x2 = sendTx x0 x1 noPay ((tagged $ Tuple1 x2) :: Eth_cancelAuctionFn)

--------------------------------------------------------------------------------
-- | Eth_getCurrentPriceFn
--------------------------------------------------------------------------------

type Eth_getCurrentPriceFn = Tagged (SProxy "getCurrentPrice(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

eth_getCurrentPrice :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e (UIntN (D2 :& D5 :& D6))
eth_getCurrentPrice x0 x1 cm x3 = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple1 x3) :: Eth_getCurrentPriceFn)

--------------------------------------------------------------------------------
-- | Eth_nonFungibleContractFn
--------------------------------------------------------------------------------

type Eth_nonFungibleContractFn = Tagged (SProxy "nonFungibleContract()") (Tuple0 )

eth_nonFungibleContract :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> Web3 p e Address
eth_nonFungibleContract x0 x1 cm = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple0 ) :: Eth_nonFungibleContractFn)

--------------------------------------------------------------------------------
-- | Eth_transferOwnershipFn
--------------------------------------------------------------------------------

type Eth_transferOwnershipFn = Tagged (SProxy "transferOwnership(address)") (Tuple1 Address)

eth_transferOwnership :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> Address -> Web3 p e HexString
eth_transferOwnership x0 x1 x2 = sendTx x0 x1 noPay ((tagged $ Tuple1 x2) :: Eth_transferOwnershipFn)



--------------------------------------------------------------------------------
-- | AuctionCreated
--------------------------------------------------------------------------------

newtype AuctionCreated = AuctionCreated {tokenId :: (UIntN (D2 :& D5 :& D6)),startingPrice :: (UIntN (D2 :& D5 :& D6)),endingPrice :: (UIntN (D2 :& D5 :& D6)),duration :: (UIntN (D2 :& D5 :& D6))}

derive instance newtypeAuctionCreated :: Newtype AuctionCreated _

instance eventFilterAuctionCreated :: EventFilter AuctionCreated where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just (HexString "a9c8dfcda5664a5a124c713e386da27de87432d5b668e79458501eb296389ba7")]
		# _fromBlock .~ Earliest 
		# _toBlock .~ Latest

instance indexedEventAuctionCreated :: IndexedEvent (Tuple0 ) (Tuple4 (Tagged (SProxy "tokenId") (UIntN (D2 :& D5 :& D6))) (Tagged (SProxy "startingPrice") (UIntN (D2 :& D5 :& D6))) (Tagged (SProxy "endingPrice") (UIntN (D2 :& D5 :& D6))) (Tagged (SProxy "duration") (UIntN (D2 :& D5 :& D6)))) AuctionCreated where
  isAnonymous _ = false

derive instance genericAuctionCreated :: G.Generic AuctionCreated _

instance eventGenericAuctionCreatedShow :: Show AuctionCreated where
	show = GShow.genericShow

instance eventGenericAuctionCreatedeq :: Eq AuctionCreated where
	eq = GEq.genericEq

--------------------------------------------------------------------------------
-- | AuctionSuccessful
--------------------------------------------------------------------------------

newtype AuctionSuccessful = AuctionSuccessful {tokenId :: (UIntN (D2 :& D5 :& D6)),totalPrice :: (UIntN (D2 :& D5 :& D6)),winner :: Address}

derive instance newtypeAuctionSuccessful :: Newtype AuctionSuccessful _

instance eventFilterAuctionSuccessful :: EventFilter AuctionSuccessful where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just (HexString "4fcc30d90a842164dd58501ab874a101a3749c3d4747139cefe7c876f4ccebd2")]
		# _fromBlock .~ Earliest 
		# _toBlock .~ Latest

instance indexedEventAuctionSuccessful :: IndexedEvent (Tuple0 ) (Tuple3 (Tagged (SProxy "tokenId") (UIntN (D2 :& D5 :& D6))) (Tagged (SProxy "totalPrice") (UIntN (D2 :& D5 :& D6))) (Tagged (SProxy "winner") Address)) AuctionSuccessful where
  isAnonymous _ = false

derive instance genericAuctionSuccessful :: G.Generic AuctionSuccessful _

instance eventGenericAuctionSuccessfulShow :: Show AuctionSuccessful where
	show = GShow.genericShow

instance eventGenericAuctionSuccessfuleq :: Eq AuctionSuccessful where
	eq = GEq.genericEq

--------------------------------------------------------------------------------
-- | AuctionCancelled
--------------------------------------------------------------------------------

newtype AuctionCancelled = AuctionCancelled {tokenId :: (UIntN (D2 :& D5 :& D6))}

derive instance newtypeAuctionCancelled :: Newtype AuctionCancelled _

instance eventFilterAuctionCancelled :: EventFilter AuctionCancelled where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just (HexString "2809c7e17bf978fbc7194c0a694b638c4215e9140cacc6c38ca36010b45697df")]
		# _fromBlock .~ Earliest 
		# _toBlock .~ Latest

instance indexedEventAuctionCancelled :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "tokenId") (UIntN (D2 :& D5 :& D6)))) AuctionCancelled where
  isAnonymous _ = false

derive instance genericAuctionCancelled :: G.Generic AuctionCancelled _

instance eventGenericAuctionCancelledShow :: Show AuctionCancelled where
	show = GShow.genericShow

instance eventGenericAuctionCancelledeq :: Eq AuctionCancelled where
	eq = GEq.genericEq

--------------------------------------------------------------------------------
-- | Pause
--------------------------------------------------------------------------------

newtype Pause = Pause {}

derive instance newtypePause :: Newtype Pause _

instance eventFilterPause :: EventFilter Pause where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just (HexString "6985a02210a168e66602d3235cb6db0e70f92b3ba4d376a33c0f3d9434bff625")]
		# _fromBlock .~ Earliest 
		# _toBlock .~ Latest

instance indexedEventPause :: IndexedEvent (Tuple0 ) (Tuple0 ) Pause where
  isAnonymous _ = false

derive instance genericPause :: G.Generic Pause _

instance eventGenericPauseShow :: Show Pause where
	show = GShow.genericShow

instance eventGenericPauseeq :: Eq Pause where
	eq = GEq.genericEq

--------------------------------------------------------------------------------
-- | Unpause
--------------------------------------------------------------------------------

newtype Unpause = Unpause {}

derive instance newtypeUnpause :: Newtype Unpause _

instance eventFilterUnpause :: EventFilter Unpause where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just (HexString "7805862f689e2f13df9f062ff482ad3ad112aca9e0847911ed832e158c525b33")]
		# _fromBlock .~ Earliest 
		# _toBlock .~ Latest

instance indexedEventUnpause :: IndexedEvent (Tuple0 ) (Tuple0 ) Unpause where
  isAnonymous _ = false

derive instance genericUnpause :: G.Generic Unpause _

instance eventGenericUnpauseShow :: Show Unpause where
	show = GShow.genericShow

instance eventGenericUnpauseeq :: Eq Unpause where
	eq = GEq.genericEq
