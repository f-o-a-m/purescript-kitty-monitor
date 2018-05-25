module Main where

import Debug.Trace
import Debug.Trace
import Prelude

import App.Config (ckAddress, tokenContract) as Config
import Contracts.CryptoKitties as CK
import Contracts.KittyCore as KC
import Control.Monad.Aff (Aff, Milliseconds(..), delay, launchAff)
import Control.Monad.Aff.AVar (AVAR, makeEmptyVar, putVar, takeVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Reader (ask, ReaderT)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import Data.Array (fold, replicate)
import Data.Either (Either(..))
import Data.Foldable (maximumBy)
import Data.Formatter.Number (Formatter(..), format)
import Data.Lens ((.~))
import Data.Lens (Lens', Prism', lens, over, prism, prism')
import Data.List (List(..), length, take, unsnoc)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (unwrap, wrap)
import Data.String (fromCharArray)
import Data.String as Str
import Data.Tuple (Tuple(..), uncurry)
import Math ((%))
import Network.Ethereum.Web3 (Address, BigNumber, ChainCursor(..), Change(..), ETH, EventAction(..), HexString, Metamask, Web3, _fromBlock, _toBlock, embed, event, eventFilter, forkWeb3', metamask, mkAddress, mkHexString, runWeb3)
import Network.Ethereum.Web3.Api (eth_blockNumber, eth_getBalance)
import Network.Ethereum.Web3.Api (eth_getBalance)
import Network.Ethereum.Web3.Solidity (unUIntN)
import Network.Ethereum.Web3.Types (BlockNumber)
import Partial.Unsafe (unsafePartial)
import React (ReactElement)
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

data KittenAction
  = SelectUserAddress Address
  | ImageStateChange ImageAction

appClass :: R.ReactClass KittyProps
appClass = R.createClass kittyTransfersSpec

--------------------------------------------------------------------------------
type KittenEffects eff = (eth :: ETH | eff)

type ImageState =
  { baseURL :: String
  , loadTryCount :: Int
  , loadStatus :: ImageLoadState
  }

data ImageLoadState = Loading | Loaded | Failed
data ImageAction = LoadFailed | LoadSucceeded | RetryLoading

initialImageState :: String -> ImageState
initialImageState baseURL =
  { baseURL
  , loadTryCount: 100
  , loadStatus: Loading
  }

-- TODO: use P.onError once this PR is merged
-- https://github.com/purescript-contrib/purescript-react/pull/133
onError :: forall eff props state result.
  (R.Event -> R.EventHandlerContext eff props state result) -> P.Props
onError f = P.unsafeMkProps "onError" (R.handle f)

imageSpec :: forall eff props . T.Spec (KittenEffects eff) ImageState props ImageAction
imageSpec = T.simpleSpec performAction render

  where
  performAction :: T.PerformAction (KittenEffects eff) ImageState props ImageAction
  performAction action _ _ = void $ T.modifyState \s -> case action of
    LoadFailed ->
      if s.loadTryCount > 0
        then
          s { loadTryCount = s.loadTryCount - 1}
        else
          s { loadStatus = Failed }
    LoadSucceeded ->
      s { loadStatus = Loaded}
    RetryLoading ->
      initialImageState s.baseURL

  render :: T.Render ImageState props ImageAction
  render dispatch props state _ =
    let
      classNameModifiers = case state.loadStatus of
        Loading -> "Image--loading"
        Loaded -> "Image--loaded"
        Failed -> "Image--failed"
    in
      pure $ D.div
        [ P.className ("Image " <> classNameModifiers) ]
        $ case state.loadStatus of
            Loading ->
              renderImage dispatch state
            Loaded ->
              renderImage dispatch state
            Failed ->
              [ D.div
                  [ P.className "Image-error"
                  , P.onClick \_ -> dispatch $ RetryLoading
                  ]
                  []
              ]

  renderImage dispatch state =
    pure $ D.img
      [ P.className "Image-img"
      , P.src $ state.baseURL <> "try=" <> show state.loadTryCount
      , onError \_ -> dispatch LoadFailed
      , P.onLoad \_ -> dispatch LoadSucceeded
      ]
      []

_KittenToImageAction :: Prism' KittenAction ImageAction
_KittenToImageAction = prism ImageStateChange \ta ->
  case ta of
    ImageStateChange c -> Right c
    _ -> Left ta

_imageState :: Lens' Kitten ImageState
_imageState = lens _.imageState (_ { imageState = _ })


transferSpec :: forall eff props . T.Spec (KittenEffects eff) Kitten props KittenAction
transferSpec = transferContainer $ fold
  [ imageContainer $ T.focus _imageState _KittenToImageAction imageSpec
  , T.simpleSpec T.defaultPerformAction renderTransfer
  ]
  where
  transferContainer :: forall state action eff'. T.Spec eff' state props action -> T.Spec eff' state props action
  transferContainer = over T._render \render d p s c ->
    [ D.div [P.className "kitty-tile"] (render d p s c) ]
  
  imageContainer :: forall state action eff'. T.Spec eff' state props action -> T.Spec eff' state props action
  imageContainer = over T._render \render d p s c ->
    [ D.div [P.className "kitty-pic"] (render d p s c) ]

  renderTransfer :: T.Render Kitten props KittenAction
  renderTransfer dispatch props state _ =
    [ D.div [P.className "kitty-info"]
        [ D.div [P.className "kitty-info-headings"]
            [ D.h6 [] [ D.text $ "to: " ]
            , D.h6 [] [ D.text $ "from: " ]
            , D.h6 [] [ D.text $ "tokenId: " ]
            , D.h6 [] [ D.text $ "transactionHash: " ]
            , D.h6 [] [ D.text $ "blockNumber: " ]
            ]
        , D.div [ P.className "kitty-info-details" ]
            [ D.h5 [ P.className "user-address-link", P.onClick (\_ -> dispatch $ SelectUserAddress state.to)  ] [ addressLink state.to ]
            , D.h5 [ P.className "user-address-link", P.onClick (\_ -> dispatch $ SelectUserAddress state.from)] [ addressLink state.from]
            , D.h5 [] [ D.text state.tokenId ]
            , D.h5 [] [ D.a [ P.href $ "https://etherscan.io/tx/" <> show state.txHash, P.target "_blank" ]
                            [ D.text $ shortenLink $ show state.txHash]
                      ]
            , D.h5 [] [ D.text $ show $ state.blockNumber ]
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
  
  -- This value is used as part of kitten image url to fix #10
  , imageState :: ImageState
  }

type UserInfo =
  { address :: Address
  , ethBalance :: BigNumber
  , tokenBalance :: BigNumber
  , transferCount :: Int
  , receiveCount :: Int
  }

type TransferListState =
  { transfers :: List Kitten
  , selectedUser :: Maybe Address
  , userInfoMap :: Map.Map Address UserInfo
  , transferCount :: Int
  , blockNumber :: Maybe BlockNumber
  }

data TransferListAction = KittenAction Int KittenAction

_TransferListAction :: Prism' TransferListAction (Tuple Int KittenAction)
_TransferListAction = prism' (uncurry KittenAction) \ta ->
  case ta of
    KittenAction i a -> Just (Tuple i a)

_firstTenTransfers :: Lens' TransferListState (List Kitten)
_firstTenTransfers = lens (_.transfers >>> take 10) (_ { transfers = _ })

transferListSpec :: forall eff props. T.Spec (KittenEffects eff) TransferListState props TransferListAction
transferListSpec = fold
    [ cardList $ T.withState \st ->
        T.focus _firstTenTransfers _TransferListAction $
          T.foreach \_ -> transferSpec
    , userInfoBox
    , listActions
    ]
  where

    cardList :: T.Spec (KittenEffects eff) TransferListState props TransferListAction
             -> T.Spec (KittenEffects eff) TransferListState props TransferListAction
    cardList = over T._render \render dispatch p s c ->
      [ D.div [P.className "kitty-container"]
        [ D.div [P.className "kitty-list"] $
            render dispatch p s c
        ]
      ]

    userInfoBox :: T.Spec (KittenEffects eff) TransferListState props TransferListAction
    userInfoBox = T.simpleSpec T.defaultPerformAction render
      where
        render dispatch props state _ =
          [ D.div [P.className "monitor-stats"] $  monitorStats state <> userInfoDiv (state.selectedUser >>= flip Map.lookup state.userInfoMap) ]



        monitorStats state =
          [ D.h2 [] [ D.text "Monitor Stats" ]
          , D.h6 [] [ D.text $ "current block number: " <> maybe "loading ..." show state.blockNumber ]
          , D.h6 [] [ D.text $ "Transfers since start: " <> show state.transferCount ]
          , D.h6 [] [ D.text $ "Biggest transfer since start: " <> "(erc20 only)"]
          , D.h6 [] [ D.span' $ maybe [ D.text "Most xfers: waiting for more xfers... " ]
                                (\user -> [ D.text "Most xfers: ", addressLink user.address, D.text $ " with " <> show user.transferCount <> " so far!" ])
                                (findMostTransfers state.userInfoMap)
                    ]
          , D.h6 [] [ D.span' $ maybe [ D.text $ "Biggest Hodler: waiting for more xfers... " ]
                                (\user -> [ D.text "Biggest Hodler: ", addressLink user.address, D.text $ " with " <> show user.tokenBalance <> " kitties!" ])
                                (findBiggestBalance state.userInfoMap)
                    ]
          , D.h6 [] [ Map.filter (\user -> user.transferCount > 0) state.userInfoMap
                      # Map.size
                      # show
                      # flip (<>) " unique addresses have done transfers so far"
                      # D.text
                    ]
          ]

        userInfoDiv :: Maybe UserInfo -> Array ReactElement
        userInfoDiv mUserInfo =
          case mUserInfo of
          Nothing ->
            [ D.h5 [] [ D.text $ "Click an address to get user info"] ]
          Just userInfo ->
            [ D.h2 [] [ D.text "User Info" ]
            , D.h6 [] $ [ addressLink userInfo.address ]
            , D.h6 [] [ D.text $ "Eth Balance: " <> (addDecimalPointAt 18 $ show userInfo.ethBalance) ]
            , D.h6 [] [ D.text $ "Has " <> show userInfo.tokenBalance <> " kitties!" ]
            ]


    monitorStats :: T.Spec (eth :: ETH | eff) TransferListState props TransferListAction
    monitorStats = T.simpleSpec T.defaultPerformAction render
      where
        render dispatch props state _ =
          [D.div [P.className "monitor-stats"] [D.text $ show state.transferCount]]

    listActions :: T.Spec (KittenEffects eff) TransferListState props TransferListAction
    listActions = T.simpleSpec performAction T.defaultRender
      where
        performAction :: T.PerformAction (KittenEffects eff) TransferListState props TransferListAction
        performAction (KittenAction i (ImageStateChange _)) _ _ = pure unit
        performAction (KittenAction i (SelectUserAddress address)) _ _ = pure unit
        performAction (KittenAction i (SelectUserAddress userAddress)) _ state =
          void $ T.modifyState _{ selectedUser = Just userAddress }



kittyTransfersSpec :: forall eff props. R.ReactSpec props TransferListState (eth :: ETH | eff)
kittyTransfersSpec =
    let {spec} = T.createReactSpec transferListSpec initialState
    in spec { componentDidMount = monitorKitties
            }
  where
    initialState = const $ pure { transfers: Nil
                                , selectedUser: Nothing
                                , transferCount: zero
                                , userInfoMap: Map.empty
                                , blockNumber: Nothing
                                }

    monitorKitties :: R.ComponentDidMount props TransferListState (KittenEffects eff)
    monitorKitties this = void $ do
      void <<< launchAff $ runWeb3 metamask $ do
          currentBn <- getAndSetBlockNumber this
          pullBlockNumber this
          tokAddress <- Config.tokenContract
          let startingBn = wrap <<< (\bn -> bn - embed 10) <<< unwrap $ currentBn
              fltr = eventFilter (Proxy :: Proxy KC.Transfer) tokAddress # _fromBlock .~ BN startingBn
          event fltr $ \t -> do
            ch@(Change c) <- ask
            st <- liftEff $ R.readState this
            let  st' = st { blockNumber = max (Just c.blockNumber) st.blockNumber }
            newState <- insertTransfer st' tokAddress (BN c.blockNumber) t
            void $ liftEff $ R.writeState this newState
            pure ContinueEvent
    
    getAndSetBlockNumber this = do
      num <- eth_blockNumber
      traceAnyA { num }
      _ <- liftEff $ R.transformState this (_ { blockNumber = Just num })
      pure num
    
    pullBlockNumber this = 
      void $ forkWeb3' metamask do
        liftAff $ delay (Milliseconds 500.0)
        _ <- getAndSetBlockNumber this
        pullBlockNumber this


    insertTransfer :: forall eff1 . TransferListState -> Address -> ChainCursor -> KC.Transfer -> ReaderT Change (Web3 Metamask eff1) TransferListState
    insertTransfer st tokAddress c (KC.Transfer t) = do
      (Change change) <- ask -- Ask for Filter Changes
      let transfer = { to: t.to
                     , from: t.from
                     , tokenId: show t.tokenId
                     , txHash: change.transactionHash
                     , blockNumber: change.blockNumber
                     , toBalance: Nothing
                     , fromBalance: Nothing
                     , imageState: initialImageState $ "https://storage.googleapis.com/ck-kitty-image/0x06012c8cf97bead5deae237070f9587f8e7a266d/" <> show t.tokenId <> ".svg?"
                     }

      let transfers' | length st.transfers <= 20 = st.transfers
                     | otherwise = (unsafePartial fromJust $ unsnoc st.transfers).init

      --userInfoMap' <- lift $ do
      --  receiver <- updateReceiver Config.ckAddress t.to c st.userInfoMap
      --  updateTransferer tokAddress t.from c receiver
          
          -- filter out kitties coming from 0x0000000000000000000000000000000000000000
          transfers'' = if t.from == unsafePartial fromJust (mkAddress =<< mkHexString "0x0000000000000000000000000000000000000000")
                          then transfers'
                          else Cons transfer $ transfers'

      pure  st { transfers = transfers''
               , transferCount = st.transferCount + 1
       --        , userInfoMap = userInfoMap'
               }

-- Utils
updateTransferer :: forall eff.
                    Address
                 -> Address
                 -> ChainCursor
                 -> Map.Map Address UserInfo
                 -> Web3 Metamask eff (Map.Map Address UserInfo)
updateTransferer  tokenContract transfererAddress c users = do
  balance <- KC.eth_balanceOf tokenContract Nothing c transfererAddress
  ethBalance <- eth_getBalance transfererAddress c
  let updateUserValue userInfo = Just $
        userInfo { ethBalance = ethBalance
                 , tokenBalance = unUIntN balance
                 , transferCount = userInfo.transferCount + 1
                 }
  if Map.member transfererAddress users
    then pure $ Map.update updateUserValue transfererAddress users
    else pure $ Map.insert transfererAddress
      { address: transfererAddress
      , ethBalance: ethBalance
      , tokenBalance: unUIntN balance
      , transferCount: 1
      , receiveCount: 0
      } users

updateReceiver :: forall eff.
                  Address
               -> Address
               -> ChainCursor
               -> Map.Map Address UserInfo
               -> Web3 Metamask eff (Map.Map Address UserInfo)
updateReceiver  tokenContract receiverAddress c users = do
  balance <- KC.eth_balanceOf tokenContract Nothing c receiverAddress
  ethBalance <- eth_getBalance receiverAddress c
  let updateUserValue userInfo = Just $
        userInfo { ethBalance = ethBalance
                 , tokenBalance = unUIntN balance
                 , receiveCount = userInfo.transferCount + 1
                 }
  if Map.member receiverAddress users
    then pure $ Map.update updateUserValue receiverAddress users
    else pure $ Map.insert receiverAddress
      { address: receiverAddress
      , ethBalance: ethBalance
      , tokenBalance: unUIntN balance
      , transferCount: 0
      , receiveCount: 1
      } users


shortenLink :: String -> String
shortenLink str | Str.length str < 20 = str
                | otherwise  = short
  where
    short = Str.take 7 str <> "..." <> Str.drop (Str.length str - 5) str

addressLink :: Address -> ReactElement
addressLink address =
   D.a [ P.href $ "https://etherscan.io/address/" <> show address, P.target "_blank" ]
        [ D.text $ show address ]


addDecimalPointAt :: Int -> String -> String
addDecimalPointAt decimalAmount stringyNum =
  case strLength <= decimalAmount of
    true -> "0." <> (fromCharArray $ replicate ltOrEDiff '0') <> stringyNum
    false -> (Str.take gtDiff stringyNum) <> "." <> (Str.drop gtDiff stringyNum)
  where
    strLength = Str.length stringyNum
    ltOrEDiff = decimalAmount - strLength
    gtDiff = strLength - decimalAmount


findMostTransfers :: Map.Map Address UserInfo -> Maybe UserInfo
findMostTransfers userInfoMap =
  case maxTransfersUser of
    Nothing ->
      Nothing
    Just userInfo ->
      if userInfo.transferCount < 2 then Nothing else Just userInfo
  where
    maxTransfersUser = maximumBy (\u1 u2 -> compare u1.transferCount u2.transferCount) userInfoMap


findBiggestBalance :: Map.Map Address UserInfo -> Maybe UserInfo
findBiggestBalance userInfoMap =
  maximumBy (\u1 u2 -> compare u1.tokenBalance u2.tokenBalance) userInfoMap

avgNumFormat :: Formatter
avgNumFormat = Formatter { comma: false, before: 0, after: 1, abbreviations: false, sign: false }
