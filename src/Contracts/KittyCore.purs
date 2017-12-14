module Contracts.KittyCore where

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
-- | Eth_cfoAddressFn
--------------------------------------------------------------------------------

type Eth_cfoAddressFn = Tagged (SProxy "cfoAddress()") (Tuple0 )

eth_cfoAddress :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> Web3 p e Address
eth_cfoAddress x0 x1 cm = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple0 ) :: Eth_cfoAddressFn)

--------------------------------------------------------------------------------
-- | Eth_tokenMetadataFn
--------------------------------------------------------------------------------

type Eth_tokenMetadataFn = Tagged (SProxy "tokenMetadata(uint256,string)") (Tuple2 (UIntN (D2 :& D5 :& D6)) String)

eth_tokenMetadata :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> (UIntN (D2 :& D5 :& D6)) -> String -> Web3 p e String
eth_tokenMetadata x0 x1 cm x3 x4 = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple2 x3 x4) :: Eth_tokenMetadataFn)

--------------------------------------------------------------------------------
-- | Eth_promoCreatedCountFn
--------------------------------------------------------------------------------

type Eth_promoCreatedCountFn = Tagged (SProxy "promoCreatedCount()") (Tuple0 )

eth_promoCreatedCount :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> Web3 p e (UIntN (D2 :& D5 :& D6))
eth_promoCreatedCount x0 x1 cm = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple0 ) :: Eth_promoCreatedCountFn)

--------------------------------------------------------------------------------
-- | Eth_nameFn
--------------------------------------------------------------------------------

type Eth_nameFn = Tagged (SProxy "name()") (Tuple0 )

eth_name :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> Web3 p e String
eth_name x0 x1 cm = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple0 ) :: Eth_nameFn)

--------------------------------------------------------------------------------
-- | Eth_approveFn
--------------------------------------------------------------------------------

type Eth_approveFn = Tagged (SProxy "approve(address,uint256)") (Tuple2 Address (UIntN (D2 :& D5 :& D6)))

eth_approve :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> Address -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e HexString
eth_approve x0 x1 x2 x3 = sendTx x0 x1 noPay ((tagged $ Tuple2 x2 x3) :: Eth_approveFn)

--------------------------------------------------------------------------------
-- | Eth_ceoAddressFn
--------------------------------------------------------------------------------

type Eth_ceoAddressFn = Tagged (SProxy "ceoAddress()") (Tuple0 )

eth_ceoAddress :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> Web3 p e Address
eth_ceoAddress x0 x1 cm = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple0 ) :: Eth_ceoAddressFn)

--------------------------------------------------------------------------------
-- | Eth_GEN0_STARTING_PRICEFn
--------------------------------------------------------------------------------

type Eth_GEN0_STARTING_PRICEFn = Tagged (SProxy "GEN0_STARTING_PRICE()") (Tuple0 )

eth_GEN0_STARTING_PRICE :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> Web3 p e (UIntN (D2 :& D5 :& D6))
eth_GEN0_STARTING_PRICE x0 x1 cm = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple0 ) :: Eth_GEN0_STARTING_PRICEFn)

--------------------------------------------------------------------------------
-- | Eth_setSiringAuctionAddressFn
--------------------------------------------------------------------------------

type Eth_setSiringAuctionAddressFn = Tagged (SProxy "setSiringAuctionAddress(address)") (Tuple1 Address)

eth_setSiringAuctionAddress :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> Address -> Web3 p e HexString
eth_setSiringAuctionAddress x0 x1 x2 = sendTx x0 x1 noPay ((tagged $ Tuple1 x2) :: Eth_setSiringAuctionAddressFn)

--------------------------------------------------------------------------------
-- | Eth_totalSupplyFn
--------------------------------------------------------------------------------

type Eth_totalSupplyFn = Tagged (SProxy "totalSupply()") (Tuple0 )

eth_totalSupply :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> Web3 p e (UIntN (D2 :& D5 :& D6))
eth_totalSupply x0 x1 cm = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple0 ) :: Eth_totalSupplyFn)

--------------------------------------------------------------------------------
-- | Eth_pregnantKittiesFn
--------------------------------------------------------------------------------

type Eth_pregnantKittiesFn = Tagged (SProxy "pregnantKitties()") (Tuple0 )

eth_pregnantKitties :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> Web3 p e (UIntN (D2 :& D5 :& D6))
eth_pregnantKitties x0 x1 cm = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple0 ) :: Eth_pregnantKittiesFn)

--------------------------------------------------------------------------------
-- | Eth_isPregnantFn
--------------------------------------------------------------------------------

type Eth_isPregnantFn = Tagged (SProxy "isPregnant(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

eth_isPregnant :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e Boolean
eth_isPregnant x0 x1 cm x3 = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple1 x3) :: Eth_isPregnantFn)

--------------------------------------------------------------------------------
-- | Eth_GEN0_AUCTION_DURATIONFn
--------------------------------------------------------------------------------

type Eth_GEN0_AUCTION_DURATIONFn = Tagged (SProxy "GEN0_AUCTION_DURATION()") (Tuple0 )

eth_GEN0_AUCTION_DURATION :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> Web3 p e (UIntN (D2 :& D5 :& D6))
eth_GEN0_AUCTION_DURATION x0 x1 cm = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple0 ) :: Eth_GEN0_AUCTION_DURATIONFn)

--------------------------------------------------------------------------------
-- | Eth_siringAuctionFn
--------------------------------------------------------------------------------

type Eth_siringAuctionFn = Tagged (SProxy "siringAuction()") (Tuple0 )

eth_siringAuction :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> Web3 p e Address
eth_siringAuction x0 x1 cm = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple0 ) :: Eth_siringAuctionFn)

--------------------------------------------------------------------------------
-- | Eth_transferFromFn
--------------------------------------------------------------------------------

type Eth_transferFromFn = Tagged (SProxy "transferFrom(address,address,uint256)") (Tuple3 Address Address (UIntN (D2 :& D5 :& D6)))

eth_transferFrom :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> Address -> Address -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e HexString
eth_transferFrom x0 x1 x2 x3 x4 = sendTx x0 x1 noPay ((tagged $ Tuple3 x2 x3 x4) :: Eth_transferFromFn)

--------------------------------------------------------------------------------
-- | Eth_setGeneScienceAddressFn
--------------------------------------------------------------------------------

type Eth_setGeneScienceAddressFn = Tagged (SProxy "setGeneScienceAddress(address)") (Tuple1 Address)

eth_setGeneScienceAddress :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> Address -> Web3 p e HexString
eth_setGeneScienceAddress x0 x1 x2 = sendTx x0 x1 noPay ((tagged $ Tuple1 x2) :: Eth_setGeneScienceAddressFn)

--------------------------------------------------------------------------------
-- | Eth_setCEOFn
--------------------------------------------------------------------------------

type Eth_setCEOFn = Tagged (SProxy "setCEO(address)") (Tuple1 Address)

eth_setCEO :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> Address -> Web3 p e HexString
eth_setCEO x0 x1 x2 = sendTx x0 x1 noPay ((tagged $ Tuple1 x2) :: Eth_setCEOFn)

--------------------------------------------------------------------------------
-- | Eth_setCOOFn
--------------------------------------------------------------------------------

type Eth_setCOOFn = Tagged (SProxy "setCOO(address)") (Tuple1 Address)

eth_setCOO :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> Address -> Web3 p e HexString
eth_setCOO x0 x1 x2 = sendTx x0 x1 noPay ((tagged $ Tuple1 x2) :: Eth_setCOOFn)

--------------------------------------------------------------------------------
-- | Eth_createSaleAuctionFn
--------------------------------------------------------------------------------

type Eth_createSaleAuctionFn = Tagged (SProxy "createSaleAuction(uint256,uint256,uint256,uint256)") (Tuple4 (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6)))

eth_createSaleAuction :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> (UIntN (D2 :& D5 :& D6)) -> (UIntN (D2 :& D5 :& D6)) -> (UIntN (D2 :& D5 :& D6)) -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e HexString
eth_createSaleAuction x0 x1 x2 x3 x4 x5 = sendTx x0 x1 noPay ((tagged $ Tuple4 x2 x3 x4 x5) :: Eth_createSaleAuctionFn)

--------------------------------------------------------------------------------
-- | Eth_unpauseFn
--------------------------------------------------------------------------------

type Eth_unpauseFn = Tagged (SProxy "unpause()") (Tuple0 )

eth_unpause :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> Web3 p e HexString
eth_unpause x0 x1 = sendTx x0 x1 noPay ((tagged $ Tuple0 ) :: Eth_unpauseFn)

--------------------------------------------------------------------------------
-- | Eth_sireAllowedToAddressFn
--------------------------------------------------------------------------------

type Eth_sireAllowedToAddressFn = Tagged (SProxy "sireAllowedToAddress(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

eth_sireAllowedToAddress :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e Address
eth_sireAllowedToAddress x0 x1 cm x3 = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple1 x3) :: Eth_sireAllowedToAddressFn)

--------------------------------------------------------------------------------
-- | Eth_canBreedWithFn
--------------------------------------------------------------------------------

type Eth_canBreedWithFn = Tagged (SProxy "canBreedWith(uint256,uint256)") (Tuple2 (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6)))

eth_canBreedWith :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> (UIntN (D2 :& D5 :& D6)) -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e Boolean
eth_canBreedWith x0 x1 cm x3 x4 = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple2 x3 x4) :: Eth_canBreedWithFn)

--------------------------------------------------------------------------------
-- | Eth_kittyIndexToApprovedFn
--------------------------------------------------------------------------------

type Eth_kittyIndexToApprovedFn = Tagged (SProxy "kittyIndexToApproved(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

eth_kittyIndexToApproved :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e Address
eth_kittyIndexToApproved x0 x1 cm x3 = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple1 x3) :: Eth_kittyIndexToApprovedFn)

--------------------------------------------------------------------------------
-- | Eth_createSiringAuctionFn
--------------------------------------------------------------------------------

type Eth_createSiringAuctionFn = Tagged (SProxy "createSiringAuction(uint256,uint256,uint256,uint256)") (Tuple4 (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6)))

eth_createSiringAuction :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> (UIntN (D2 :& D5 :& D6)) -> (UIntN (D2 :& D5 :& D6)) -> (UIntN (D2 :& D5 :& D6)) -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e HexString
eth_createSiringAuction x0 x1 x2 x3 x4 x5 = sendTx x0 x1 noPay ((tagged $ Tuple4 x2 x3 x4 x5) :: Eth_createSiringAuctionFn)

--------------------------------------------------------------------------------
-- | Eth_setAutoBirthFeeFn
--------------------------------------------------------------------------------

type Eth_setAutoBirthFeeFn = Tagged (SProxy "setAutoBirthFee(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

eth_setAutoBirthFee :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e HexString
eth_setAutoBirthFee x0 x1 x2 = sendTx x0 x1 noPay ((tagged $ Tuple1 x2) :: Eth_setAutoBirthFeeFn)

--------------------------------------------------------------------------------
-- | Eth_approveSiringFn
--------------------------------------------------------------------------------

type Eth_approveSiringFn = Tagged (SProxy "approveSiring(address,uint256)") (Tuple2 Address (UIntN (D2 :& D5 :& D6)))

eth_approveSiring :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> Address -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e HexString
eth_approveSiring x0 x1 x2 x3 = sendTx x0 x1 noPay ((tagged $ Tuple2 x2 x3) :: Eth_approveSiringFn)

--------------------------------------------------------------------------------
-- | Eth_setCFOFn
--------------------------------------------------------------------------------

type Eth_setCFOFn = Tagged (SProxy "setCFO(address)") (Tuple1 Address)

eth_setCFO :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> Address -> Web3 p e HexString
eth_setCFO x0 x1 x2 = sendTx x0 x1 noPay ((tagged $ Tuple1 x2) :: Eth_setCFOFn)

--------------------------------------------------------------------------------
-- | Eth_createPromoKittyFn
--------------------------------------------------------------------------------

type Eth_createPromoKittyFn = Tagged (SProxy "createPromoKitty(uint256,address)") (Tuple2 (UIntN (D2 :& D5 :& D6)) Address)

eth_createPromoKitty :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> (UIntN (D2 :& D5 :& D6)) -> Address -> Web3 p e HexString
eth_createPromoKitty x0 x1 x2 x3 = sendTx x0 x1 noPay ((tagged $ Tuple2 x2 x3) :: Eth_createPromoKittyFn)

--------------------------------------------------------------------------------
-- | Eth_setSecondsPerBlockFn
--------------------------------------------------------------------------------

type Eth_setSecondsPerBlockFn = Tagged (SProxy "setSecondsPerBlock(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

eth_setSecondsPerBlock :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e HexString
eth_setSecondsPerBlock x0 x1 x2 = sendTx x0 x1 noPay ((tagged $ Tuple1 x2) :: Eth_setSecondsPerBlockFn)

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
-- | Eth_ownerOfFn
--------------------------------------------------------------------------------

type Eth_ownerOfFn = Tagged (SProxy "ownerOf(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

eth_ownerOf :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e Address
eth_ownerOf x0 x1 cm x3 = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple1 x3) :: Eth_ownerOfFn)

--------------------------------------------------------------------------------
-- | Eth_GEN0_CREATION_LIMITFn
--------------------------------------------------------------------------------

type Eth_GEN0_CREATION_LIMITFn = Tagged (SProxy "GEN0_CREATION_LIMIT()") (Tuple0 )

eth_GEN0_CREATION_LIMIT :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> Web3 p e (UIntN (D2 :& D5 :& D6))
eth_GEN0_CREATION_LIMIT x0 x1 cm = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple0 ) :: Eth_GEN0_CREATION_LIMITFn)

--------------------------------------------------------------------------------
-- | Eth_newContractAddressFn
--------------------------------------------------------------------------------

type Eth_newContractAddressFn = Tagged (SProxy "newContractAddress()") (Tuple0 )

eth_newContractAddress :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> Web3 p e Address
eth_newContractAddress x0 x1 cm = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple0 ) :: Eth_newContractAddressFn)

--------------------------------------------------------------------------------
-- | Eth_setSaleAuctionAddressFn
--------------------------------------------------------------------------------

type Eth_setSaleAuctionAddressFn = Tagged (SProxy "setSaleAuctionAddress(address)") (Tuple1 Address)

eth_setSaleAuctionAddress :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> Address -> Web3 p e HexString
eth_setSaleAuctionAddress x0 x1 x2 = sendTx x0 x1 noPay ((tagged $ Tuple1 x2) :: Eth_setSaleAuctionAddressFn)

--------------------------------------------------------------------------------
-- | Eth_balanceOfFn
--------------------------------------------------------------------------------

type Eth_balanceOfFn = Tagged (SProxy "balanceOf(address)") (Tuple1 Address)

eth_balanceOf :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> Address -> Web3 p e (UIntN (D2 :& D5 :& D6))
eth_balanceOf x0 x1 cm x3 = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple1 x3) :: Eth_balanceOfFn)

--------------------------------------------------------------------------------
-- | Eth_setNewAddressFn
--------------------------------------------------------------------------------

type Eth_setNewAddressFn = Tagged (SProxy "setNewAddress(address)") (Tuple1 Address)

eth_setNewAddress :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> Address -> Web3 p e HexString
eth_setNewAddress x0 x1 x2 = sendTx x0 x1 noPay ((tagged $ Tuple1 x2) :: Eth_setNewAddressFn)

--------------------------------------------------------------------------------
-- | Eth_secondsPerBlockFn
--------------------------------------------------------------------------------

type Eth_secondsPerBlockFn = Tagged (SProxy "secondsPerBlock()") (Tuple0 )

eth_secondsPerBlock :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> Web3 p e (UIntN (D2 :& D5 :& D6))
eth_secondsPerBlock x0 x1 cm = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple0 ) :: Eth_secondsPerBlockFn)

--------------------------------------------------------------------------------
-- | Eth_pauseFn
--------------------------------------------------------------------------------

type Eth_pauseFn = Tagged (SProxy "pause()") (Tuple0 )

eth_pause :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> Web3 p e HexString
eth_pause x0 x1 = sendTx x0 x1 noPay ((tagged $ Tuple0 ) :: Eth_pauseFn)

--------------------------------------------------------------------------------
-- | Eth_tokensOfOwnerFn
--------------------------------------------------------------------------------

type Eth_tokensOfOwnerFn = Tagged (SProxy "tokensOfOwner(address)") (Tuple1 Address)

eth_tokensOfOwner :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> Address -> Web3 p e (Array (UIntN (D2 :& D5 :& D6)))
eth_tokensOfOwner x0 x1 cm x3 = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple1 x3) :: Eth_tokensOfOwnerFn)

--------------------------------------------------------------------------------
-- | Eth_giveBirthFn
--------------------------------------------------------------------------------

type Eth_giveBirthFn = Tagged (SProxy "giveBirth(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

eth_giveBirth :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e HexString
eth_giveBirth x0 x1 x2 = sendTx x0 x1 noPay ((tagged $ Tuple1 x2) :: Eth_giveBirthFn)

--------------------------------------------------------------------------------
-- | Eth_withdrawAuctionBalancesFn
--------------------------------------------------------------------------------

type Eth_withdrawAuctionBalancesFn = Tagged (SProxy "withdrawAuctionBalances()") (Tuple0 )

eth_withdrawAuctionBalances :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> Web3 p e HexString
eth_withdrawAuctionBalances x0 x1 = sendTx x0 x1 noPay ((tagged $ Tuple0 ) :: Eth_withdrawAuctionBalancesFn)

--------------------------------------------------------------------------------
-- | Eth_symbolFn
--------------------------------------------------------------------------------

type Eth_symbolFn = Tagged (SProxy "symbol()") (Tuple0 )

eth_symbol :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> Web3 p e String
eth_symbol x0 x1 cm = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple0 ) :: Eth_symbolFn)

--------------------------------------------------------------------------------
-- | Eth_cooldownsFn
--------------------------------------------------------------------------------

type Eth_cooldownsFn = Tagged (SProxy "cooldowns(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

eth_cooldowns :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e (UIntN (D3 :& D2))
eth_cooldowns x0 x1 cm x3 = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple1 x3) :: Eth_cooldownsFn)

--------------------------------------------------------------------------------
-- | Eth_kittyIndexToOwnerFn
--------------------------------------------------------------------------------

type Eth_kittyIndexToOwnerFn = Tagged (SProxy "kittyIndexToOwner(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

eth_kittyIndexToOwner :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e Address
eth_kittyIndexToOwner x0 x1 cm x3 = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple1 x3) :: Eth_kittyIndexToOwnerFn)

--------------------------------------------------------------------------------
-- | Eth_transferFn
--------------------------------------------------------------------------------

type Eth_transferFn = Tagged (SProxy "transfer(address,uint256)") (Tuple2 Address (UIntN (D2 :& D5 :& D6)))

eth_transfer :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> Address -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e HexString
eth_transfer x0 x1 x2 x3 = sendTx x0 x1 noPay ((tagged $ Tuple2 x2 x3) :: Eth_transferFn)

--------------------------------------------------------------------------------
-- | Eth_cooAddressFn
--------------------------------------------------------------------------------

type Eth_cooAddressFn = Tagged (SProxy "cooAddress()") (Tuple0 )

eth_cooAddress :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> Web3 p e Address
eth_cooAddress x0 x1 cm = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple0 ) :: Eth_cooAddressFn)

--------------------------------------------------------------------------------
-- | Eth_autoBirthFeeFn
--------------------------------------------------------------------------------

type Eth_autoBirthFeeFn = Tagged (SProxy "autoBirthFee()") (Tuple0 )

eth_autoBirthFee :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> Web3 p e (UIntN (D2 :& D5 :& D6))
eth_autoBirthFee x0 x1 cm = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple0 ) :: Eth_autoBirthFeeFn)

--------------------------------------------------------------------------------
-- | Eth_erc721MetadataFn
--------------------------------------------------------------------------------

type Eth_erc721MetadataFn = Tagged (SProxy "erc721Metadata()") (Tuple0 )

eth_erc721Metadata :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> Web3 p e Address
eth_erc721Metadata x0 x1 cm = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple0 ) :: Eth_erc721MetadataFn)

--------------------------------------------------------------------------------
-- | Eth_createGen0AuctionFn
--------------------------------------------------------------------------------

type Eth_createGen0AuctionFn = Tagged (SProxy "createGen0Auction(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

eth_createGen0Auction :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e HexString
eth_createGen0Auction x0 x1 x2 = sendTx x0 x1 noPay ((tagged $ Tuple1 x2) :: Eth_createGen0AuctionFn)

--------------------------------------------------------------------------------
-- | Eth_isReadyToBreedFn
--------------------------------------------------------------------------------

type Eth_isReadyToBreedFn = Tagged (SProxy "isReadyToBreed(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

eth_isReadyToBreed :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e Boolean
eth_isReadyToBreed x0 x1 cm x3 = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple1 x3) :: Eth_isReadyToBreedFn)

--------------------------------------------------------------------------------
-- | Eth_PROMO_CREATION_LIMITFn
--------------------------------------------------------------------------------

type Eth_PROMO_CREATION_LIMITFn = Tagged (SProxy "PROMO_CREATION_LIMIT()") (Tuple0 )

eth_PROMO_CREATION_LIMIT :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> Web3 p e (UIntN (D2 :& D5 :& D6))
eth_PROMO_CREATION_LIMIT x0 x1 cm = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple0 ) :: Eth_PROMO_CREATION_LIMITFn)

--------------------------------------------------------------------------------
-- | Eth_setMetadataAddressFn
--------------------------------------------------------------------------------

type Eth_setMetadataAddressFn = Tagged (SProxy "setMetadataAddress(address)") (Tuple1 Address)

eth_setMetadataAddress :: forall e p . IsAsyncProvider p => Maybe Address -> Address -> Address -> Web3 p e HexString
eth_setMetadataAddress x0 x1 x2 = sendTx x0 x1 noPay ((tagged $ Tuple1 x2) :: Eth_setMetadataAddressFn)

--------------------------------------------------------------------------------
-- | Eth_saleAuctionFn
--------------------------------------------------------------------------------

type Eth_saleAuctionFn = Tagged (SProxy "saleAuction()") (Tuple0 )

eth_saleAuction :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> Web3 p e Address
eth_saleAuction x0 x1 cm = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple0 ) :: Eth_saleAuctionFn)

--------------------------------------------------------------------------------
-- | Eth_getKittyFn
--------------------------------------------------------------------------------

type Eth_getKittyFn = Tagged (SProxy "getKitty(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

eth_getKitty :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e (Tuple10 Boolean Boolean (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6)))
eth_getKitty x0 x1 cm x3 = call x0 x1 cm ((tagged $ Tuple1 x3) :: Eth_getKittyFn)

--------------------------------------------------------------------------------
-- | Eth_bidOnSiringAuctionFn
--------------------------------------------------------------------------------

type Eth_bidOnSiringAuctionFn = Tagged (SProxy "bidOnSiringAuction(uint256,uint256)") (Tuple2 (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6)))

eth_bidOnSiringAuction :: forall e p u . IsAsyncProvider p => EtherUnit u => Maybe Address -> Address -> u -> (UIntN (D2 :& D5 :& D6)) -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e HexString
eth_bidOnSiringAuction x0 x1 u x3 x4 = sendTx x0 x1 u ((tagged $ Tuple2 x3 x4) :: Eth_bidOnSiringAuctionFn)

--------------------------------------------------------------------------------
-- | Eth_gen0CreatedCountFn
--------------------------------------------------------------------------------

type Eth_gen0CreatedCountFn = Tagged (SProxy "gen0CreatedCount()") (Tuple0 )

eth_gen0CreatedCount :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> Web3 p e (UIntN (D2 :& D5 :& D6))
eth_gen0CreatedCount x0 x1 cm = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple0 ) :: Eth_gen0CreatedCountFn)

--------------------------------------------------------------------------------
-- | Eth_geneScienceFn
--------------------------------------------------------------------------------

type Eth_geneScienceFn = Tagged (SProxy "geneScience()") (Tuple0 )

eth_geneScience :: forall e p . IsAsyncProvider p => Address -> Maybe Address -> BlockMode -> Web3 p e Address
eth_geneScience x0 x1 cm = unTuple1 <$> call x0 x1 cm ((tagged $ Tuple0 ) :: Eth_geneScienceFn)

--------------------------------------------------------------------------------
-- | Eth_breedWithAutoFn
--------------------------------------------------------------------------------

type Eth_breedWithAutoFn = Tagged (SProxy "breedWithAuto(uint256,uint256)") (Tuple2 (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6)))

eth_breedWithAuto :: forall e p u . IsAsyncProvider p => EtherUnit u => Maybe Address -> Address -> u -> (UIntN (D2 :& D5 :& D6)) -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e HexString
eth_breedWithAuto x0 x1 u x3 x4 = sendTx x0 x1 u ((tagged $ Tuple2 x3 x4) :: Eth_breedWithAutoFn)





--------------------------------------------------------------------------------
-- | Pregnant
--------------------------------------------------------------------------------

newtype Pregnant = Pregnant {owner :: Address,matronId :: (UIntN (D2 :& D5 :& D6)),sireId :: (UIntN (D2 :& D5 :& D6)),cooldownEndBlock :: (UIntN (D2 :& D5 :& D6))}

derive instance newtypePregnant :: Newtype Pregnant _

instance eventFilterPregnant :: EventFilter Pregnant where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just (HexString "241ea03ca20251805084d27d4440371c34a0b85ff108f6bb5611248f73818b80")]
		# _fromBlock .~ Earliest 
		# _toBlock .~ Latest

instance indexedEventPregnant :: IndexedEvent (Tuple0 ) (Tuple4 (Tagged (SProxy "owner") Address) (Tagged (SProxy "matronId") (UIntN (D2 :& D5 :& D6))) (Tagged (SProxy "sireId") (UIntN (D2 :& D5 :& D6))) (Tagged (SProxy "cooldownEndBlock") (UIntN (D2 :& D5 :& D6)))) Pregnant where
  isAnonymous _ = false

derive instance genericPregnant :: G.Generic Pregnant _

instance eventGenericPregnantShow :: Show Pregnant where
	show = GShow.genericShow

instance eventGenericPregnanteq :: Eq Pregnant where
	eq = GEq.genericEq

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

--------------------------------------------------------------------------------
-- | Birth
--------------------------------------------------------------------------------

newtype Birth = Birth {owner :: Address,kittyId :: (UIntN (D2 :& D5 :& D6)),matronId :: (UIntN (D2 :& D5 :& D6)),sireId :: (UIntN (D2 :& D5 :& D6)),genes :: (UIntN (D2 :& D5 :& D6))}

derive instance newtypeBirth :: Newtype Birth _

instance eventFilterBirth :: EventFilter Birth where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just (HexString "0a5311bd2a6608f08a180df2ee7c5946819a649b204b554bb8e39825b2c50ad5")]
		# _fromBlock .~ Earliest 
		# _toBlock .~ Latest

instance indexedEventBirth :: IndexedEvent (Tuple0 ) (Tuple5 (Tagged (SProxy "owner") Address) (Tagged (SProxy "kittyId") (UIntN (D2 :& D5 :& D6))) (Tagged (SProxy "matronId") (UIntN (D2 :& D5 :& D6))) (Tagged (SProxy "sireId") (UIntN (D2 :& D5 :& D6))) (Tagged (SProxy "genes") (UIntN (D2 :& D5 :& D6)))) Birth where
  isAnonymous _ = false

derive instance genericBirth :: G.Generic Birth _

instance eventGenericBirthShow :: Show Birth where
	show = GShow.genericShow

instance eventGenericBirtheq :: Eq Birth where
	eq = GEq.genericEq

--------------------------------------------------------------------------------
-- | ContractUpgrade
--------------------------------------------------------------------------------

newtype ContractUpgrade = ContractUpgrade {newContract :: Address}

derive instance newtypeContractUpgrade :: Newtype ContractUpgrade _

instance eventFilterContractUpgrade :: EventFilter ContractUpgrade where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just (HexString "450db8da6efbe9c22f2347f7c2021231df1fc58d3ae9a2fa75d39fa446199305")]
		# _fromBlock .~ Earliest 
		# _toBlock .~ Latest

instance indexedEventContractUpgrade :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "newContract") Address)) ContractUpgrade where
  isAnonymous _ = false

derive instance genericContractUpgrade :: G.Generic ContractUpgrade _

instance eventGenericContractUpgradeShow :: Show ContractUpgrade where
	show = GShow.genericShow

instance eventGenericContractUpgradeeq :: Eq ContractUpgrade where
	eq = GEq.genericEq
