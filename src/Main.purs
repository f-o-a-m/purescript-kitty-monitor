module Main where

import Prelude

import Contracts.CryptoKitties as CK
import Contracts.KittyCore as KC
import Control.Monad.Aff (Milliseconds(..), delay, launchAff, liftEff')
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Reader (ask)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import Data.Array (fold)
import Data.Lens (Lens', Prism', lens, prism', over)
import Data.List (List(..), length, take, unsnoc)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..), uncurry)
import Network.Ethereum.Web3 (Address, CallMode(..), Change(..), ETH, EventAction(..), HexString, event, metamask, mkAddress, mkHexString, runWeb3)
import Partial.Unsafe (unsafePartial)
import React as R
import React.DOM as D
import React.DOM.Props as P
import ReactDOM (render)
import Thermite as T

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

type KittenAction = Unit

appClass :: R.ReactClass KittyProps
appClass = R.createClass kittyTransfersSpec

--------------------------------------------------------------------------------

transferSpec :: forall eff props . T.Spec eff Kitten props KittenAction
transferSpec = T.simpleSpec T.defaultPerformAction render
  where
    render :: T.Render Kitten props KittenAction
    render _ props transfer _ =
      [ D.div [] [ D.div [] [D.text $ "to: " <> show transfer.to]
                 , D.div [] [D.text $ "from: " <> show transfer.from]
                 , D.div [] [D.text $ "tokenId: " <> show transfer.tokenId]
                 , D.div [] [D.text $ "transactionHash: " <> show transfer.txHash]
                 , D.div [] [D.text $ "blockNumber: " <> show transfer.blockNumber]
                 , D.object [ P._type "image/svg+xml"
                            , P.width "500px"
                            , P.height "500px"
                            , P.unsafeMkProps "data" $ "https://storage.googleapis.com/ck-kitty-image/0x06012c8cf97bead5deae237070f9587f8e7a266d/" <> transfer.tokenId <> ".svg"
                            ] []
                 ]
      ]


type Kitten =
  { to :: Address
  , from :: Address
  , tokenId :: String
  , txHash :: HexString
  , blockNumber :: HexString
  }

type TransferListState =
  { transfers :: List Kitten
  }


data TransferListAction = KittenAction Int KittenAction

_TransferListAction :: Prism' TransferListAction (Tuple Int KittenAction)
_TransferListAction = prism' (uncurry KittenAction) \ta ->
  case ta of
    KittenAction i a -> Just (Tuple i a)

_transfers :: Lens' TransferListState (List Kitten)
_transfers = lens _.transfers (_ { transfers = _ })

transferListSpec :: forall eff props action. T.Spec eff TransferListState props TransferListAction
transferListSpec = fold
    [ cardList $ T.withState  \st ->
        T.focus _transfers _TransferListAction $
          T.foreach \_ -> transferSpec
    ]
  where
    cardList :: T.Spec eff TransferListState props TransferListAction
             -> T.Spec eff TransferListState props TransferListAction
    cardList = over T._render \render dispatch p s c ->
      [ D.div [] $
        render dispatch p s c
      ]
    listActions :: T.Spec eff TransferListState props TransferListAction
    listActions = T.simpleSpec T.defaultPerformAction T.defaultRender

kittyTransfersSpec :: forall eff props. R.ReactSpec props TransferListState (eth :: ETH, console :: CONSOLE | eff)
kittyTransfersSpec =
    let {spec} = T.createReactSpec transferListSpec (const $ pure {transfers: Nil})
    in spec {componentDidMount = monitorKitties}
  where
    monitorKitties :: R.ComponentDidMount props TransferListState (eth :: ETH, console :: CONSOLE | eff)
    monitorKitties this = void $ do
      props <- R.getProps this
      launchAff $ do
        delay (Milliseconds 1000.0)
        void $ runWeb3 metamask $ do
          let ckAddress = unsafePartial fromJust $ mkAddress =<< mkHexString "0xC7af99Fe5513eB6710e6D5f44F9989dA40F27F26"
          aaAddress <- CK.eth_nonFungibleContract ckAddress Nothing Latest
          event aaAddress $ \(KC.Transfer t) -> do
            liftAff $ delay (Milliseconds 15000.0)
            liftEff <<< log $ "Looking for Kitten: " <> show t.tokenId
            (Change change) <- ask
            let ev = { to: t.to
                     , from: t.from
                     , tokenId: show t.tokenId
                     , txHash: change.transactionHash
                     , blockNumber: change.blockNumber
                     }
            st  <- liftEff $ R.readState this
            let st' = if length st.transfers <= 10
                        then st.transfers
                        else (unsafePartial fromJust $ unsnoc st.transfers).init
            _ <- liftEff <<< R.transformState this $ \st -> st {transfers = Cons ev $ st' }
            pure ContinueEvent
