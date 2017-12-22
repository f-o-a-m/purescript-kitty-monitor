module Main where

import Prelude

import Contracts.CryptoKitties as CK
import Contracts.KittyCore as KC
import Control.Monad.Aff (Milliseconds(..), delay, launchAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import Data.Array (fold, replicate)
import Data.DateTime.Instant (unInstant)
import Data.Formatter.Number (Formatter(..), format)
import Data.Int (toNumber)
import Data.Lens (Lens', Prism', lens, over, prism')
import Data.List (List(..), length, take, unsnoc)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (unwrap)
import Data.String (fromCharArray)
import Data.String as Str
import Data.Tuple (Tuple(..), uncurry)
import Math ((%))
import Network.Ethereum.Web3 (Address, BigNumber, ChainCursor(..), Change(..), ETH, EventAction(..), HexString, event, eventFilter, metamask, mkAddress, mkHexString, runWeb3)
import Network.Ethereum.Web3.Api (eth_getBalance)
import Network.Ethereum.Web3.Solidity (unUIntN)
import Network.Ethereum.Web3.Types (BlockNumber)
import Partial.Unsafe (unsafePartial)
import React as R
import React.DOM as D
import React.DOM.Props as P
import ReactDOM (render)
import Thermite as T
import Type.Proxy (Proxy(..))

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

data KittenAction = SelectUserAddress Address

appClass :: R.ReactClass KittyProps
appClass = R.createClass kittyTransfersSpec

--------------------------------------------------------------------------------

transferSpec :: forall eff props . T.Spec (eth :: ETH, console :: CONSOLE | eff) Kitten props KittenAction
transferSpec = T.simpleSpec T.defaultPerformAction render
  where
    render :: T.Render Kitten props KittenAction
    render dispatch props state _ =
        [ D.div [ P.className "kitty-tile" ]
          [ D.a [ P.className "kitty-pic", P.href $ "https://etherscan.io/tx/" <> show state.txHash, P.target "_blank" ]
                [ D.img  [ P._type "image/svg+xml"
                         , P.width "500px"
                         , P.height "500px"
                         , P.src $ "https://storage.googleapis.com/ck-kitty-image/0x06012c8cf97bead5deae237070f9587f8e7a266d/" <> state.tokenId <> ".svg"
                         ] []
                ]
          , D.div [P.className "kitty-info"]
             [ D.div [P.className "kitty-info-headings"]
                 [ D.h6 [] [ D.text $ "to: " ]
                 , D.h6 [] [ D.text $ "from: " ]
                 , D.h6 [] [ D.text $ "tokenId: " ]
                 , D.h6 [] [ D.text $ "transactionHash: " ]
                 , D.h6 [] [ D.text $ "blockNumber: " ]
                 ]
             , D.div [ P.className "kitty-info-details" ]
                 [ D.h5 [ P.className "user-address-link", P.onClick (\_ -> dispatch $ SelectUserAddress state.to)  ] [ D.text $ show state.to ]
                 , D.h5 [ P.className "user-address-link", P.onClick (\_ -> dispatch $ SelectUserAddress state.from)] [ D.text $ show state.from]
                 , D.h5 [] [ D.text state.tokenId ]
                 , D.h5 [] [ D.a [ P.href $ "https://etherscan.io/tx/" <> show state.txHash, P.target "_blank" ]
                                 [ D.text $ shortenLink $ show state.txHash]
                           ]
                 , D.h5 [] [ D.text $ show $ state.blockNumber ]
                 ]
             ]
          ]
        ]


type Kitten =
  { to :: Address
  , from :: Address
  , tokenId :: String
  , txHash :: HexString
  , blockNumber :: BlockNumber
  , toBalance :: Maybe BigNumber
  , fromBalance :: Maybe BigNumber
  }

type UserInfo =
  { address :: Address
  , ethBalance :: BigNumber
  , tokenBalance :: BigNumber
  }

type TransferListState =
  { transfers :: List Kitten
  , userInfo :: Maybe UserInfo
  , transferCount :: Number
  , startTime :: Number
  , transferRate :: Number
  }

data TransferListAction = KittenAction Int KittenAction

_TransferListAction :: Prism' TransferListAction (Tuple Int KittenAction)
_TransferListAction = prism' (uncurry KittenAction) \ta ->
  case ta of
    KittenAction i a -> Just (Tuple i a)

_firstTenTransfers :: Lens' TransferListState (List Kitten)
_firstTenTransfers = lens (_.transfers >>> take 10) (_ { transfers = _ })

transferListSpec :: forall eff props action. T.Spec (eth :: ETH, console :: CONSOLE | eff) TransferListState props TransferListAction
transferListSpec = fold
    [ cardList $ T.withState \st ->
        T.focus _firstTenTransfers _TransferListAction $
          T.foreach \_ -> transferSpec
    , infoBox
    , listActions
    ]
  where
    cardList :: T.Spec (eth :: ETH, console :: CONSOLE | eff) TransferListState props TransferListAction
             -> T.Spec (eth :: ETH, console :: CONSOLE | eff) TransferListState props TransferListAction
    cardList = over T._render \render dispatch p s c ->
      [ D.div [P.className "kitty-container"]
        [ D.div [P.className "kitty-list"] $
          render dispatch p s c
        ]
      ]

    infoBox :: T.Spec (eth :: ETH, console :: CONSOLE | eff) TransferListState props TransferListAction
    infoBox = T.simpleSpec T.defaultPerformAction render
      where
        render dispatch props state _ =
          maybe ([D.div [P.className "monitor-stats"] $ monitorStats state]) (\userInfo ->
            [ D.div [P.className "monitor-stats"] $  monitorStats state <> userInfoDiv userInfo ]
          ) state.userInfo

        monitorStats state =
          [ D.h2 [] [ D.text "Monitor Stats" ]
          , D.h6 [] [ D.text $ "Transfers since start: " <> format countNumFormat state.transferCount ]
          , D.h6 [] [ D.text $ "Avg transfers/minute: " <>
                               if state.transferRate == zero
                               then "calculating..."
                               else format avgNumFormat state.transferRate
                    ]
          , D.h6 [] [ D.text $ "Biggest transfer since start: " <> "(erc20 only)"]
          , D.h5 [] [ D.text $ "Click an address to get user info"]
          ]

        userInfoDiv userInfo =
          [ D.h2 [] [ D.text "User Info" ]
          , D.h6 [] [ D.a [P.href $ "https://etherscan.io/address/" <> show userInfo.address, P.target "_blank"]
                          [D.text $ "User: " <> (shortenLink $ show userInfo.address)]
                    ]
          , D.h6 [] [ D.text $ "Eth Balance: " <> (addDecimalPointAt 18 $ show userInfo.ethBalance) ]
          , D.h6 [] [ D.text $ "Has " <> show userInfo.tokenBalance <> " kitties!" ]
          ]


    monitorStats :: T.Spec (eth :: ETH, console :: CONSOLE | eff) TransferListState props TransferListAction
    monitorStats = T.simpleSpec T.defaultPerformAction render
      where
        render dispatch props state _ =
          [D.div [P.className "monitor-stats"] [D.text $ show state.transferCount]]



    listActions :: T.Spec (eth :: ETH, console :: CONSOLE | eff) TransferListState props TransferListAction
    listActions = T.simpleSpec performAction T.defaultRender
      where
        performAction :: T.PerformAction (eth :: ETH, console :: CONSOLE | eff) TransferListState props TransferListAction
        performAction (KittenAction i (SelectUserAddress address)) _ _ = do
          let kittyCoreAddress = unsafePartial fromJust $ mkAddress =<< mkHexString "0x06012c8cf97bead5deae237070f9587f8e7a266d"
          kittyBalance <- lift $ runWeb3 metamask $ KC.eth_balanceOf kittyCoreAddress Nothing Latest address
          ethBalance <- lift $ runWeb3 metamask $ eth_getBalance address Latest
          lift $ liftEff $ log "Getting to balance..."
          let userInfo = Just { address: address, tokenBalance: unUIntN kittyBalance, ethBalance: ethBalance }
          void $ T.modifyState _{ userInfo = userInfo }
        performAction _ _ _ = pure unit


kittyTransfersSpec :: forall eff props. R.ReactSpec props TransferListState (eth :: ETH, console :: CONSOLE, now :: NOW | eff)
kittyTransfersSpec =
    let {spec} = T.createReactSpec transferListSpec
          (const $ pure {transfers: Nil, userInfo: Nothing, transferCount: zero, startTime: zero, transferRate: zero})
    in spec {componentDidMount = monitorKitties}
  where
    monitorKitties :: R.ComponentDidMount props TransferListState (eth :: ETH, console :: CONSOLE, now :: NOW | eff)
    monitorKitties this = void $ do
      props <- R.getProps this
      launchAff $ do
        delay (Milliseconds 1000.0)
        void $ runWeb3 metamask $ do
          let ckAddress = unsafePartial fromJust $ mkAddress =<< mkHexString "0xc7af99fe5513eb6710e6d5f44f9989da40f27f26"
          aaAddress <- CK.eth_nonFungibleContract ckAddress Nothing Latest
          let transferFilter = eventFilter (Proxy :: Proxy KC.Transfer) aaAddress
          liftEff $ log "starting kitty watcher..."
          event transferFilter $ \(KC.Transfer t) -> do
            liftAff $ delay (Milliseconds 100.0)
            liftEff <<< log $ "Looking for Kitten: " <> show t.tokenId
            (Change change) <- ask
            st  <- liftEff $ R.readState this
            time <- liftEff $ (unwrap <<< unInstant) <$> now
            let transfer = { to: t.to
                     , from: t.from
                     , tokenId: show t.tokenId
                     , txHash: change.transactionHash
                     , blockNumber: change.blockNumber
                     , toBalance: Nothing
                     , fromBalance: Nothing
                     }

            let startTime' | st.startTime == zero = time
                           | otherwise = st.startTime

            let transferRate' | st.transferCount == zero = zero
                              | st.transferCount % toNumber 10 == zero =
                                  st.transferCount / ((time - st.startTime) / toNumber 60000)
                              | otherwise = st.transferRate

            let transfers' | length st.transfers <= 200 = st.transfers
                           | otherwise = (unsafePartial fromJust $ unsnoc st.transfers).init

            _ <- liftEff <<< R.transformState this $ \st -> st
              { transfers = Cons transfer $ transfers'
              , transferCount = st.transferCount + (toNumber 1)
              , startTime = startTime'
              , transferRate = transferRate'
              }
            pure ContinueEvent


-- Utils



shortenLink :: String -> String
shortenLink str | Str.length str < 20 = str
                | otherwise  = shorten str
  where
    shorten str = Str.take 7 str <> "..." <> Str.drop (Str.length str - 5) str

addDecimalPointAt :: Int -> String -> String
addDecimalPointAt decimalAmount stringyNum =
  case strLength <= decimalAmount of
    true -> "0." <> (fromCharArray $ replicate ltOrEDiff '0') <> stringyNum
    false -> (Str.take gtDiff stringyNum) <> "." <> (Str.drop gtDiff stringyNum)
  where
    strLength = Str.length stringyNum
    ltOrEDiff = decimalAmount - strLength
    gtDiff = strLength - decimalAmount

avgNumFormat :: Formatter
avgNumFormat = Formatter { comma: false, before: 0, after: 1, abbreviations: false, sign: false }

countNumFormat :: Formatter
countNumFormat = Formatter { comma: false, before: 0, after: 0, abbreviations: false, sign: false }
