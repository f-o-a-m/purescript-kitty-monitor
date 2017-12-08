module Main where

import Prelude

import Contracts.CryptoKitties as CK
import Contracts.KittyCore as KC
import Control.Monad.Aff (Milliseconds(..), delay, launchAff)
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
import Data.String as Str
import Data.Array (fold)
import Data.Lens (Lens', Prism', lens, prism', over)
import Data.List (List(..), length, unsnoc)
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
      [ D.div [P.className "kitty-tile"]
        [ D.img  [ P._type "image/svg+xml"
                 , P.width "500px"
                 , P.height "500px"
                 , P.src $ "https://storage.googleapis.com/ck-kitty-image/0x06012c8cf97bead5deae237070f9587f8e7a266d/" <> transfer.tokenId <> ".svg"
                 ] []
        , D.div [P.className "kitty-info"]
           [ D.div [P.className "kitty-info-headings"]
               [ D.h6 [] [D.text $ "to: "]
               , D.h6 [] [D.text $ "from: "]
               , D.h6 [] [D.text $ "tokenId: "]
               , D.h6 [] [D.text $ "transactionHash: "]
               , D.h6 [] [D.text $ "blockNumber: "]
               ]
           , D.div [P.className "kitty-info-details"]
               [ D.h5 [] [D.text $ show transfer.to]
               , D.h5 [] [D.text $ show transfer.from]
               , D.h5 [] [D.text $ show transfer.tokenId]
               , D.h5 [] [ D.a [ P.href $ "https://etherscan.io/tx/" <> show transfer.txHash, P.target "_blank" ]
                               [ D.text $ shortenLink $ show transfer.txHash]
                         ]
               , D.h5 [] [D.text $ show transfer.blockNumber]
               ]
           ]
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
    [ cardList $ T.withState \st ->
        T.focus _transfers _TransferListAction $
          T.foreach \_ -> transferSpec
    ]
  where
    cardList :: T.Spec eff TransferListState props TransferListAction
             -> T.Spec eff TransferListState props TransferListAction
    cardList = over T._render \render dispatch p s c ->
      [ D.div [P.className "kitty-container"]
        [ D.div [P.className "kitty-list"] $
          render dispatch p s c
        ]
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
          liftEff $ log "starting kitty watcher..."
          event aaAddress $ \(KC.Transfer t) -> do
            liftAff $ delay (Milliseconds 25000.0)
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


-- Utils

shortenLink :: String -> String
shortenLink str | Str.length str < 20 = str
                 | otherwise  = shorten str
  where
    shorten str = Str.take 7 str <> "..." <> Str.drop (Str.length str - 5) str
