module Main where

import Prelude

import Contracts.ERC20 as ERC20
import Control.Monad.Aff (Milliseconds(..), delay, launchAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import Data.Array (fold, replicate)
import Data.Lens (Lens', Prism', lens, prism', over)
import Data.List (List(..), length, unsnoc)
import Data.Maybe (Maybe(..), fromJust, isNothing, maybe)
import Data.String (fromCharArray)
import Data.String as Str
import Data.Tuple (Tuple(..), uncurry)
import Network.Ethereum.Web3 (Address, BigNumber, CallMode(..), Change(..), ETH, EventAction(..), HexString, event, fromHexString, metamask, mkAddress, mkHexString, runWeb3, unsafeToInt)
import Network.Ethereum.Web3.Solidity (unUIntN)
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

type TokenTxState = Unit

type TokenTxProps = Unit

data TokenTxAction
    = GetToBalance Address
    | GetFromBalance Address

appClass :: R.ReactClass TokenTxProps
appClass = R.createClass erc20TransferSpec

--------------------------------------------------------------------------------

transferSpec :: forall eff props . T.Spec (eth :: ETH, console :: CONSOLE | eff) TokenTx props TokenTxAction
transferSpec = T.simpleSpec performAction render
  where
    render :: T.Render TokenTx props TokenTxAction
    render dispatch props state _ =
        [ D.div [ P.className "token-tile"]
          [ D.a [ P.className "token-pic", P.href $ "https://etherscan.io/tx/" <> show state.txHash, P.target "_blank" ]
                [ D.img  [ P._type "image/svg+xml"
                         , P.width "200px"
                         , P.height "200px"
                         , P.src $ "https://i.warosu.org/data/biz/img/0048/45/1512645890588.png"
                         ] []
                ]
          , D.div [P.className "token-info"]
             [ D.div [P.className "token-info-headings"]
                 [ D.h6 [] [ D.text $ "to: " ]
                 , D.h6 [] [ D.text $ "toBalance: " ]
                 , D.h6 [] [ D.text $ "from: " ]
                 , D.h6 [] [ D.text $ "fromBalance: " ]
                 , D.h6 [] [ D.text $ "transactionHash: " ]
                 , D.h6 [] [ D.text $ "txAmount: " ]
                 , D.h6 [] [ D.text $ "blockNumber: " ]
                 ]
             , D.div [ P.className "token-info-details" ]
                 [ D.h5 [] [ D.a [P.href $ "https://etherscan.io/address/" <> show state.to, P.target "_blank" ]
                                 [ D.text $ show state.to ] ]
                 , D.h5 (if isNothing state.toBalance then
                           [ P.className "cursor-pointer", P.onClick (\_ -> dispatch $ GetToBalance state.to) ]
                         else [])
                        [ D.text $ maybe "Get TO balance" (addDecimalPointAt state.tokenDecimals <<< show) state.toBalance ]
                 , D.h5 [] [ D.a [P.href $ "https://etherscan.io/address/" <> show state.from, P.target "_blank" ]
                                 [D.text $ show state.from] ]
                 , D.h5 (if isNothing state.fromBalance then
                           [ P.className "cursor-pointer", P.onClick (\_ -> dispatch $ GetFromBalance state.from) ]
                         else [])
                        [D.text $ maybe "Get FROM balance" (addDecimalPointAt state.tokenDecimals <<< show) state.fromBalance]
                 , D.h5 [] [ D.a [ P.href $ "https://etherscan.io/tx/" <> show state.txHash, P.target "_blank" ]
                                 [ D.text $ shortenLink $ show state.txHash]
                           ]
                 , D.h5 [] [ D.text $ addDecimalPointAt state.tokenDecimals $ show state.txAmount ]
                 , D.h5 [] [ D.text $ show $ fromHexString state.blockNumber ]
                 ]
             ]
          ]
        ]

    performAction :: T.PerformAction (eth :: ETH, console :: CONSOLE | eff) TokenTx props TokenTxAction
    performAction (GetToBalance toAddress) _ _ = do
      let erc20Contract = unsafePartial fromJust $ mkAddress =<< mkHexString "0x4156d3342d5c385a87d264f90653733592000581"
      balance <- lift $ runWeb3 metamask $ ERC20.eth_balanceOf erc20Contract Nothing Latest toAddress
      lift $ liftEff $ log "Getting to balance..."
      void $ T.modifyState $ _{toBalance = Just (unUIntN balance)}

    performAction (GetFromBalance fromAddress) _ _ = do
      let erc20Contract = unsafePartial fromJust $ mkAddress =<< mkHexString "0x4156d3342d5c385a87d264f90653733592000581"
      balance <- lift $ runWeb3 metamask $ ERC20.eth_balanceOf erc20Contract Nothing Latest fromAddress
      lift $ liftEff $ log "Getting from balance..."
      void $ T.modifyState $ _{fromBalance = Just (unUIntN balance)}




type TokenTx =
  { to :: Address
  , from :: Address
  , txHash :: HexString
  , txAmount :: BigNumber
  , blockNumber :: HexString
  , toBalance :: Maybe BigNumber
  , fromBalance :: Maybe BigNumber
  , tokenDecimals :: Int
  }

type TransferListState =
  { transfers :: List TokenTx
  }

data TransferListAction = KittenAction Int TokenTxAction

_TransferListAction :: Prism' TransferListAction (Tuple Int TokenTxAction)
_TransferListAction = prism' (uncurry KittenAction) \ta ->
  case ta of
    KittenAction i a -> Just (Tuple i a)

_transfers :: Lens' TransferListState (List TokenTx)
_transfers = lens _.transfers (_ { transfers = _ })

transferListSpec :: forall eff props action. T.Spec (eth :: ETH, console :: CONSOLE | eff) TransferListState props TransferListAction
transferListSpec = fold
    [ cardList $ T.withState \st ->
        T.focus _transfers _TransferListAction $
          T.foreach \_ -> transferSpec
    ]
  where
    cardList :: T.Spec (eth :: ETH, console :: CONSOLE | eff) TransferListState props TransferListAction
             -> T.Spec (eth :: ETH, console :: CONSOLE | eff) TransferListState props TransferListAction
    cardList = over T._render \render dispatch p s c ->
      [ D.div [P.className "token-container"]
        [ D.div [P.className "token-list"] $
          render dispatch p s c
        ]
      ]
    listActions :: T.Spec (eth :: ETH | eff) TransferListState props TransferListAction
    listActions = T.simpleSpec T.defaultPerformAction T.defaultRender

erc20TransferSpec :: forall eff props. R.ReactSpec props TransferListState (eth :: ETH, console :: CONSOLE | eff)
erc20TransferSpec =
    let {spec} = T.createReactSpec transferListSpec (const $ pure {transfers: Nil})
    in spec {componentDidMount = monitorTokens}
  where
    monitorTokens :: R.ComponentDidMount props TransferListState (eth :: ETH, console :: CONSOLE | eff)
    monitorTokens this = void $ do
      props <- R.getProps this
      launchAff $ do
        delay (Milliseconds 1000.0)
        void $ runWeb3 metamask $ do
          let erc20Contract = unsafePartial fromJust $ mkAddress =<< mkHexString "0x4156d3342d5c385a87d264f90653733592000581"
          liftEff $ log "starting erc20 watcher (SALT Token)... "
          uIntDecimals <- ERC20.eth_decimals erc20Contract Nothing Latest
          let decimals = unsafeToInt $ unUIntN uIntDecimals
          event erc20Contract $ \(ERC20.Transfer t) -> do
            liftAff $ delay (Milliseconds 10000.0)
            (Change log) <- ask
            let ev = { to: t.to
                     , from: t.from
                     , txHash: log.transactionHash
                     , txAmount: unUIntN t.value
                     , blockNumber: log.blockNumber
                     , toBalance: Nothing
                     , fromBalance: Nothing
                     , tokenDecimals: decimals
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

addDecimalPointAt :: Int -> String -> String
addDecimalPointAt decimalAmount stringyNum =
  case strLength <= decimalAmount of
    true -> "0." <> (fromCharArray $ replicate ltOrEDiff '0') <> stringyNum
    false -> (Str.take gtDiff stringyNum) <> "." <> (Str.drop gtDiff stringyNum)
  where
    strLength = Str.length stringyNum
    ltOrEDiff = decimalAmount - strLength
    gtDiff = strLength - decimalAmount
