module Main where

import Prelude

import Contracts.CryptoKitties as CK
import Contracts.ERC721 as T
import Contracts.KittyCore as KC
import Control.Monad.Aff (Aff, attempt, launchAff_)
import Control.Monad.Cont.Trans (lift)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader.Class (ask)
import Data.Either (Either(Right, Left))
import Data.Foldable (sequence_)
import Data.Lens.Setter ((.~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (wrap)
import Data.Traversable (for)
import Network.Ethereum.Web3 (class IsAsyncProvider, BlockMode(..), Change(..), ETH, _fromBlock, _toBlock, embed, eventFilter, fromHexString, mkAddress, mkHexString)
import Network.Ethereum.Web3.Contract (EventAction(..), event, mkBlockNumber)
import Network.Ethereum.Web3.Provider (runWeb3)
import Network.Ethereum.Web3.Types (Web3)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))
import Utils (myProvider)

main :: Eff (console :: CONSOLE, eth :: ETH) Unit
main = launchAff_ $ do
  liftEff $ log "hello event monitor"
  for events \e -> do
    spork e myProvider
  where
    events =
      [ cryptoKitties
--      , omiseGo
--      , ethereumWhite
--      , foam
--      , lightOracle
--      , decentrEx
      ]

type EventLog = forall p aff . (IsAsyncProvider p) => Web3 p (console :: CONSOLE | aff) Unit

spork :: forall p aff b
       . Web3 p (console :: CONSOLE | aff) b
      -> Proxy p
      -> Aff (eth :: ETH, console :: CONSOLE | aff) Unit
spork theEvent provider = do
  res <- attempt $ runWeb3 provider $ do
    theEvent
  case res of
    Left e -> do
      liftEff $ log "JSON-RPC returned empty string"
      spork theEvent provider
    Right a -> do
      liftEff $ log "got proper response from JSON-RPC"

logEvent :: forall proxy a c
          . (Show a)
         => (Monad c)
         => proxy a
         -> a
         -> ReaderT Change c EventAction
logEvent _ e = pure $ unsafePerformEff $ (log $ "logEvent: " <> show e) >>= (const $ pure ContinueEvent)

-- mainnet
cryptoKitties :: EventLog
cryptoKitties = do
  --let ckAddress = unsafePartial fromJust $ mkAddress =<< mkHexString "0xC7af99Fe5513eB6710e6D5f44F9989dA40F27F26"
  let ckAddress = unsafePartial fromJust $ mkAddress =<< mkHexString "0xc7af99fe5513eb6710e6d5f44f9989da40f27f26"
  aaAddress <- CK.eth_nonFungibleContract ckAddress Nothing Latest

  liftEff <<< log $ "Hello CryptoKitties at " <> show ckAddress
  liftEff <<< log $ "ERC721 is at " <> show aaAddress

  -- sequence_
  --   [ event ckAddress $ (logEvent $ Proxy :: Proxy CK.AuctionCreated)
  --   , event ckAddress $ (logEvent $ Proxy :: Proxy CK.AuctionSuccessful)
  --   , event ckAddress $ (logEvent $ Proxy :: Proxy CK.AuctionCancelled)
  --   , event ckAddress $ (logEvent $ Proxy :: Proxy CK.Pause)
  --   , event ckAddress $ (logEvent $ Proxy :: Proxy CK.Unpause)
  --  -- , event aaAddress $ (logEvent $ Proxy :: Proxy T.Transfer)
  --   , event aaAddress $ (logEvent $ Proxy :: Proxy T.Approval)
  --   ]

  let fltTransfer = eventFilter (Proxy :: Proxy KC.Transfer) aaAddress
                      # _fromBlock .~ (BN <<< wrap <<< embed $ 4732070)
                      # _toBlock   .~ (BN <<< wrap <<< embed $ 4732072)

  _ <- event fltTransfer $ \(e@KC.Transfer r) -> do
    (Change change) <- ask
    liftEff <<< log $ "logEvent: " <> show e
    let blockNum = BN $ change.blockNumber
    liftEff <<< log $ "event at blocknumber: " <> show blockNum
    tokens <- lift $ T.eth_balanceOf aaAddress Nothing blockNum r.to
    liftEff <<< log $ "\tTokens for " <> show r.to <> " is " <> show tokens
    pure ContinueEvent

  liftEff $ log $ "Bye CryptoKitties at " <> show ckAddress

-- -- mainnet
-- ethereumWhite :: EventLog
-- ethereumWhite = do
--   let ewAddress = unsafePartial fromJust $ mkAddress =<< mkHexString  "39e505e1518813ab3834d57d06c22b2e5a7fb9f2"
--   liftEff $ log $ "Hello EthereumWhite at " <> show ewAddress
--   sequence_
--     [ event ewAddress $ (logEvent $ Proxy :: Proxy EW.Transfer)
--     , event ewAddress $ (logEvent $ Proxy :: Proxy EW.Mine)
--     , event ewAddress $ (logEvent $ Proxy :: Proxy EW.MinePoS)
--     , event ewAddress $ (logEvent $ Proxy :: Proxy EW.MineAD)
--     , event ewAddress $ (logEvent $ Proxy :: Proxy EW.Approval)
--     , event ewAddress $ (logEvent $ Proxy :: Proxy EW.SponsoredLink)
--     ]
-- 
-- -- mainnet
-- omiseGo :: EventLog
-- omiseGo = do
--   let ogAddress = unsafePartial fromJust $ mkAddress =<< mkHexString "d26114cd6EE289AccF82350c8d8487fedB8A0C07"
--   liftEff $ log $ "Hello OmiseGo at " <> show ogAddress
--   sequence_ -- i think i know what it is
--     [ event ogAddress $ (logEvent $ Proxy :: Proxy OG.Transfer)
--     , event ogAddress $ (logEvent $ Proxy :: Proxy OG.Approval)
--     , event ogAddress $ (logEvent $ Proxy :: Proxy OG.Mint)
--     , event ogAddress $ (logEvent $ Proxy :: Proxy OG.MintFinished)
--     , event ogAddress $ (logEvent $ Proxy :: Proxy OG.Pause)
--     , event ogAddress $ (logEvent $ Proxy :: Proxy OG.Unpause)
--     ]
-- 
--   liftEff $ log $ "Bye OmiseGo at " <> show ogAddress
-- 
-- 
-- -- rinkeby
-- foam :: EventLog
-- foam = do
--   let foamAddress = unsafePartial fromJust $ mkAddress =<< mkHexString "f665d4a1b6f8d9aca484d92b47f94a0764175fbf"
--   liftEff $ log $ "Hello FOAM at " <> show foamAddress
--   sequence_ $ [event foamAddress $ (logEvent $ Proxy :: Proxy BF.DeployedBeacon)]
-- 
-- -- ropsten
-- lightOracle :: EventLog
-- lightOracle = do
--   let loAddress = unsafePartial fromJust $ mkAddress =<< mkHexString "0x874c72F8FfC0E3167b17E1a553C6Af4E2E9E9fB1"
--   liftEff $ log $ "Hello LightOracle at " <> show loAddress
--   sequence_
--     [ event loAddress $ (logEvent $ Proxy :: Proxy LO.RateDelivered)
--     , event loAddress $ (logEvent $ Proxy :: Proxy LO.NewSymbol)
--     ]
-- 
-- -- mainnet
-- decentrEx :: EventLog
-- decentrEx = do
--   let dxAddress = unsafePartial fromJust $ mkAddress =<< mkHexString "bf29685856fae1e228878dfb35b280c0adcc3b05"
--   liftEff $ log $ "Hello DecentrEx at " <> show dxAddress
--   sequence_
--     [ event dxAddress $ (logEvent $ Proxy :: Proxy DX.Trade)
--     , event dxAddress $ (logEvent $ Proxy :: Proxy DX.Trade)
--     , event dxAddress $ (logEvent $ Proxy :: Proxy DX.Order)
--     , event dxAddress $ (logEvent $ Proxy :: Proxy DX.Cancel)
--     , event dxAddress $ (logEvent $ Proxy :: Proxy DX.Deposit)
--     , event dxAddress $ (logEvent $ Proxy :: Proxy DX.Withdraw)
--     ]
