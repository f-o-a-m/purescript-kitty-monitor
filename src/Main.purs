module Main where

import Prelude


import Prelude
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import React as R
import React.DOM as D
import ReactDOM (render)

--import Contracts.BeaconFactory as BF
--import Contracts.CryptoKitties as CK
--import Contracts.DecentrEx as DX
--import Contracts.ERC721 as T
--import Contracts.EthereumWhite as EW
--import Contracts.KittyCore as KC
--import Contracts.LightOracle as LO
--import Contracts.OmiseGo as OG
--import Control.Monad.Aff (Aff, attempt, launchAff_)
--import Control.Monad.Cont.Trans (lift)
--import Control.Monad.Eff (Eff)
--import Control.Monad.Eff.Class (liftEff)
--import Control.Monad.Eff.Console (CONSOLE, log)
--import Control.Monad.Eff.Unsafe (unsafePerformEff)
--import Control.Monad.Reader (ReaderT)
--import Control.Monad.Reader.Class (ask)
--import Data.Either (Either(Right, Left))
--import Data.Foldable (sequence_)
--import Data.Maybe (Maybe(..), fromJust)
--import Data.Traversable (for)
--import Network.Ethereum.Web3 (class IsAsyncProvider, CallMode(..), Change(..), ETH, fromHexString, mkAddress, mkHexString)
--import Network.Ethereum.Web3.Contract (EventAction(..), event)
--import Network.Ethereum.Web3.Provider (runWeb3)
--import Network.Ethereum.Web3.Types (Web3)
--import Partial.Unsafe (unsafePartial)
--import Type.Proxy (Proxy(..))
--import Utils (myProvider)

main :: forall eff. Eff (dom :: DOM | eff) Unit
main = void (elm' >>= render ui)
  where
    ui :: R.ReactElement
    ui = R.createFactory appClass unit

    elm' :: Eff (dom :: DOM | eff) Element
    elm' = do
      win <- window
      doc <- document win
      elm <- getElementById (ElementId "app") (documentToNonElementParentNode (htmlDocumentToDocument doc))
      pure $ unsafePartial (fromJust elm)

--------------------------------------------------------------------------------

type KittyState = Unit

type KittyProps = Unit

appSpec :: forall eff . R.ReactSpec KittyProps KittyState eff
appSpec = R.spec unit render
  where
    render _ = pure $ D.text "hello"

appClass :: R.ReactClass KittyProps
appClass = R.createClass appSpec

--type EventLog = forall p aff . (IsAsyncProvider p) => Web3 p (console :: CONSOLE | aff) Unit
--
--
---- mainnet
--cryptoKitties :: EventLog
--cryptoKitties = do
--  let ckAddress = unsafePartial fromJust $ mkAddress =<< mkHexString "0xC7af99Fe5513eB6710e6D5f44F9989dA40F27F26"
--  aaAddress <- CK.eth_nonFungibleContract ckAddress Nothing Latest
--
--  liftEff <<< log $ "Hello CryptoKitties at " <> show ckAddress
--  liftEff <<< log $ "ERC721 is at " <> show aaAddress
--
--  sequence_
--    [ event ckAddress $ (logEvent $ Proxy :: Proxy CK.AuctionCreated)
--    , event ckAddress $ (logEvent $ Proxy :: Proxy CK.AuctionSuccessful)
--    , event ckAddress $ (logEvent $ Proxy :: Proxy CK.AuctionCancelled)
--    , event ckAddress $ (logEvent $ Proxy :: Proxy CK.Pause)
--    , event ckAddress $ (logEvent $ Proxy :: Proxy CK.Unpause)
--   -- , event aaAddress $ (logEvent $ Proxy :: Proxy T.Transfer)
--    , event aaAddress $ (logEvent $ Proxy :: Proxy T.Approval)
--    ]
--
--  _ <- event aaAddress $ \(e@KC.Transfer r) -> do
--    (Change change) <- ask
--    liftEff <<< log $ "logEvent: " <> show e
--    let blockNum = BlockNumber $ fromHexString change.blockNumber
--    tokens <- lift $ T.eth_balanceOf aaAddress Nothing blockNum r.to
--    liftEff <<< log $ "\tTokens for " <> show r.to <> " is " <> show tokens
--    pure ContinueEvent
--
--  liftEff $ log $ "Bye CryptoKitties at " <> show ckAddress
