module Main where

import Prelude

import Contracts.BeaconFactory as BF
import Contracts.DecentrEx as DX
import Contracts.EthereumWhite as EW
import Contracts.LightOracle as LO
import Control.Monad (class Monad)
import Control.Monad.Aff (Aff, attempt, runAff_)
import Control.Monad.Aff.AVar (makeEmptyVar, putVar, takeVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Reader (ReaderT(..))
import Data.Either (Either(..), either)
import Data.Maybe (fromJust)
import Network.Ethereum.Web3 (Change(..), httpProvider, mkAddress, mkHexString, runWeb3)
import Network.Ethereum.Web3.Contract (EventAction(..), event)
import Network.Ethereum.Web3.Provider (runWeb3)
import Network.Ethereum.Web3.Types (Web3)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))
import Utils (myProvider)

main = do
  log "hello event monitor"
--  foam
--  lightOracle
  decentrEx
--  ethereumWhite

logEvent :: forall proxy a b c
       . (Show a)
      => (Monad c)
      => proxy a -> a -> ReaderT b c EventAction
logEvent _ e = do
        void $ pure $ unsafePerformEff $ log $ "logEvent: " <> show e
        pure ContinueEvent

ethereumWhite = do
  let ewAddress = unsafePartial fromJust $ mkAddress =<< mkHexString  "39e505e1518813ab3834d57d06c22b2e5a7fb9f2"
  void <<< runAff_ (\e -> log $ either show (\_ -> "i'm back") e) $ do 
    liftEff $ log $ "Hello EthereumWhite at " <> show ewAddress
    runWeb3 myProvider $ do
      void <<< event ewAddress $ (logEvent $ Proxy :: Proxy EW.Transfer)
      void <<< event ewAddress $ (logEvent $ Proxy :: Proxy EW.Mine)
      void <<< event ewAddress $ (logEvent $ Proxy :: Proxy EW.MinePoS)
      void <<< event ewAddress $ (logEvent $ Proxy :: Proxy EW.MineAD)
      void <<< event ewAddress $ (logEvent $ Proxy :: Proxy EW.Approval)
      event ewAddress $ (logEvent $ Proxy :: Proxy EW.SponsoredLink)

foam = do
  let foamAddress = unsafePartial fromJust $ mkAddress =<< mkHexString "9a812f42997Bf6584723053f94c68b0e0Bdaf874"
  runAff_ (\e -> log $ either show (\_ -> "i'm back") e) $ do
    liftEff $ log $ "Hello FOAM at " <> show foamAddress
    runWeb3 myProvider $ do
      event foamAddress $ (logEvent $ Proxy :: Proxy BF.DeployedBeacon)

lightOracle = do
  let loAddress = unsafePartial fromJust $ mkAddress =<< mkHexString "0x874c72F8FfC0E3167b17E1a553C6Af4E2E9E9fB1"
  runAff_ (\e -> log $ either show (\_ -> "i'm back") e) $ do
    liftEff $ log $ "Hello LightOracle at " <> show loAddress
    runWeb3 myProvider $ do
      void <<< event loAddress $ (logEvent $ Proxy :: Proxy LO.RateDelivered)
      event loAddress $ (logEvent $ Proxy :: Proxy LO.NewSymbol)

decentrEx = do
  let dxAddress = unsafePartial fromJust $ mkAddress =<< mkHexString "bf29685856fae1e228878dfb35b280c0adcc3b05"
  void <<< runAff_ (\e -> log $ either show (\_ -> "i'm back") e) $ do 
    liftEff $ log $ "Hello DecentrEx at " <> show dxAddress
    res <- attempt <<< runWeb3 myProvider $ do
      void <<< event dxAddress $ (logEvent $ Proxy :: Proxy DX.Trade)
      void <<< event dxAddress $ (logEvent $ Proxy :: Proxy DX.Trade)
      void <<< event dxAddress $ (logEvent $ Proxy :: Proxy DX.Order)
      void <<< event dxAddress $ (logEvent $ Proxy :: Proxy DX.Cancel)
      void <<< event dxAddress $ (logEvent $ Proxy :: Proxy DX.Deposit)
      event dxAddress $ (logEvent $ Proxy :: Proxy DX.Withdraw)
    case res of
      Left e -> do
        liftEff $ log "geth is not responding properly, retrying..."
        liftEff $ decentrEx
      Right a -> do
        liftEff $ log "can't happen"
        liftEff $ decentrEx
