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
import Network.Ethereum.Web3 (Change(..))
import Network.Ethereum.Web3.Contract (EventAction(..), event)
import Network.Ethereum.Web3.Provider (runWeb3)
import Network.Ethereum.Web3.Types (class Unit, Address(Address), HexString(HexString), Web3)
import Type.Proxy (Proxy(..))
import Utils (HttpProvider)


-- main :: forall e. Eff ( console :: CONSOLE
--                       , eth :: ETH 
--                       | e
--                       ) Unit
main = do
  log "hello event monitor"
--  foam
  lightOracle
--  decentrEx
--  ethereumWhite

ethereumWhite = do
  let ewAddress = Address <<< HexString $ "39e505e1518813ab3834d57d06c22b2e5a7fb9f2"
  void <<< runAff_ (\e -> log $ either show (\_ -> "i'm back") e) $ do 

    liftEff $ log $ "Hello EthereumWhite at " <> show ewAddress
    runWeb3 $ do

      void <<< event ewAddress $ \e@(EW.Transfer _from _to _value) -> do
        liftEff $ logShow $ e
        pure ContinueEvent :: ReaderT _ (Web3 HttpProvider _) _

      void <<< event ewAddress $ \e@(EW.Mine _address _reward) -> do
        liftEff $ logShow e
        pure ContinueEvent :: ReaderT _ (Web3 HttpProvider _) _

      void <<< event ewAddress $ \e@(EW.MinePoS _address _rewardPoS) -> do
        liftEff $ logShow e
        pure ContinueEvent :: ReaderT _ (Web3 HttpProvider _) _

      void <<< event ewAddress $ \e@(EW.MineAD _address _rewardAD) -> do
        liftEff $ logShow e
        pure ContinueEvent :: ReaderT _ (Web3 HttpProvider _) _

      void <<< event ewAddress $ \e@(EW.Approval _owner _spender _value) -> do
        liftEff $ logShow e
        pure ContinueEvent :: ReaderT _ (Web3 HttpProvider _) _

      event ewAddress $ \e@(EW.SponsoredLink _note) -> do
        liftEff $ logShow e
        pure ContinueEvent :: ReaderT _ (Web3 HttpProvider _) _


foam = do
  let foamAddress = Address <<< HexString $ "9a812f42997Bf6584723053f94c68b0e0Bdaf874"
  runAff_ (\e -> log $ either show (\_ -> "i'm back") e) $ do

    liftEff $ log $ "Hello FOAM at " <> show foamAddress

    runWeb3 $ do
      void <<< event foamAddress $ (magic $ Proxy :: Proxy BF.DeployedBeacon)
 
      event foamAddress $ \e@(BF.DeployedBeacon _ _ _ _) -> do
        liftEff $ logShow $ e
        pure ContinueEvent :: ReaderT _ (Web3 HttpProvider _) _

lightOracle = do
  let loAddress = Address <<< HexString $ "0x874c72F8FfC0E3167b17E1a553C6Af4E2E9E9fB1"
  runAff_ (\e -> log $ either show (\_ -> "i'm back") e) $ do

    liftEff $ log $ "Hello LightOracle at " <> show loAddress

    runWeb3 $ do
      void <<< event loAddress $ (magic $ Proxy :: Proxy LO.RateDelivered)
      
      event loAddress $ \e@(LO.NewSymbol _ _) -> do
        liftEff $ logShow $ e
        pure ContinueEvent :: ReaderT _ (Web3 HttpProvider _) _


magic :: forall proxy a b c
       . (Show a)
      => (Monad c)
      => proxy a -> a -> ReaderT b c EventAction
magic _ e = do
        void $ pure $ unsafePerformEff $ log $ "Magic! " <> show e
        --liftEff $ logShow $ e 
        pure ContinueEvent

decentrEx = do 
  let dxAddress = Address <<< HexString $ "bf29685856fae1e228878dfb35b280c0adcc3b05"
  void <<< runAff_ (\e -> log $ either show (\_ -> "i'm back") e) $ do 

    liftEff $ log $ "Hello DecentrEx at " <> show dxAddress

    res <- attempt <<< runWeb3 $ do

      void <<< event dxAddress $
        (magic $ Proxy :: Proxy DX.Trade)

      void <<< event dxAddress $ \e@(DX.Trade _ _ _ _ _ _) -> do
        liftEff $ logShow $ e
        pure ContinueEvent :: ReaderT _ (Web3 HttpProvider _) _

      void <<< event dxAddress $ \e@(DX.Order _ _ _ _ _ _ _) -> do
        liftEff $ logShow $ e
        pure ContinueEvent :: ReaderT _ (Web3 HttpProvider _) _

      void <<< event dxAddress $ \e@(DX.Cancel _ _ _ _ _ _ _ _ _ _) -> do
        liftEff $ logShow $ e
        pure ContinueEvent :: ReaderT _ (Web3 HttpProvider _) _

      void <<< event dxAddress $ \e@(DX.Deposit _ _ _ _) -> do
        liftEff $ logShow $ e
        pure ContinueEvent :: ReaderT _ (Web3 HttpProvider _) _

      event dxAddress $ \e@(DX.Withdraw _ _ _ _) -> do
        liftEff $ logShow $ e
        pure ContinueEvent :: ReaderT _ (Web3 HttpProvider _) _

    case res of
      Left e -> do
        liftEff $ log "geth is not responding properly, retrying..."
        liftEff $ decentrEx
      Right a -> do
        liftEff $ log "can't happen"
        liftEff $ decentrEx
