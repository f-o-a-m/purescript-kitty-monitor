module Main where

import Prelude

import Contracts.EthereumWhite as EW
import Contracts.DecentrEx as DX

import Control.Monad.Aff (runAff_)
import Control.Monad.Aff.AVar (makeEmptyVar, putVar, takeVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, logShow)
import Control.Monad.Reader (ReaderT(..))
import Data.Either (either)
import Network.Ethereum.Web3.Contract (EventAction(..), event)
import Network.Ethereum.Web3.Provider (runWeb3)
import Network.Ethereum.Web3.Types (Web3, Address(Address), HexString(HexString))
import Utils (HttpProvider)


-- main :: forall e. Eff ( console :: CONSOLE
--                       , eth :: ETH 
--                       | e
--                       ) Unit
main = do
  log "hellow"
  decentrEx
--  ethereumWhite

ethereumWhite = do
  let ewAddress = Address <<< HexString $ "39e505e1518813ab3834d57d06c22b2e5a7fb9f2"
  void <<< runAff_ (\e -> log $ either show (\_ -> "i'm back") e) $ do 
    varTransfer <- makeEmptyVar 
    varMine <- makeEmptyVar
    varPoS <- makeEmptyVar
    varAD <- makeEmptyVar
    varApproval <- makeEmptyVar
    varSponsored <- makeEmptyVar

    liftEff $ log $ "Hello EthereumWhite at " <> show ewAddress
    _ <- runWeb3 $ do

      void <<< event ewAddress $ \e@(EW.Transfer _from _to _value) -> do
        _ <- liftAff $ putVar _value varTransfer
        liftEff $ logShow $ e
        pure TerminateEvent :: ReaderT _ (Web3 HttpProvider _) _

      void <<< event ewAddress $ \e@(EW.Mine _address _reward) -> do
        _ <- liftAff $ putVar _reward varMine
        liftEff $ logShow e
        pure TerminateEvent :: ReaderT _ (Web3 HttpProvider _) _

      void <<< event ewAddress $ \e@(EW.MinePoS _address _rewardPoS) -> do
        _ <- liftAff $ putVar _rewardPoS varPoS
        liftEff $ logShow e
        pure TerminateEvent :: ReaderT _ (Web3 HttpProvider _) _

      void <<< event ewAddress $ \e@(EW.MineAD _address _rewardAD) -> do
        _ <- liftAff $ putVar _rewardAD varAD
        liftEff $ logShow e
        pure TerminateEvent :: ReaderT _ (Web3 HttpProvider _) _

      void <<< event ewAddress $ \e@(EW.Approval _owner _spender _value) -> do
        _ <- liftAff $ putVar _value varApproval
        liftEff $ logShow e
        pure TerminateEvent :: ReaderT _ (Web3 HttpProvider _) _

      event ewAddress $ \e@(EW.SponsoredLink _note) -> do
        _ <- liftAff $ putVar _note varSponsored
        liftEff $ logShow e
        pure TerminateEvent :: ReaderT _ (Web3 HttpProvider _) _

    printVar varTransfer
    printVar varMine
    printVar varPoS
    printVar varAD
    printVar varApproval
 --   printVar varSponsored


decentrEx = do 
  let dxAddress = Address <<< HexString $ "bf29685856fae1e228878dfb35b280c0adcc3b05"
  void <<< runAff_ (\e -> log $ either show (\_ -> "i'm back") e) $ do 
    varTrade <- makeEmptyVar
    varOrder <- makeEmptyVar
    varCancel <- makeEmptyVar
    varDeposit <- makeEmptyVar
    varWithdraw <- makeEmptyVar

    liftEff $ log $ "Hello DecentrEx at " <> show dxAddress
    _ <- runWeb3 $ do
      void <<< event dxAddress $ \e@(DX.Trade _tokenGet _amountGet _tokenGive _amountGive _get _give) -> do
        liftEff $ logShow $ e
        _ <- liftAff $ putVar _give varTrade
        pure TerminateEvent :: ReaderT _ (Web3 HttpProvider _) _

      void <<< event dxAddress $ \e@(DX.Order _ _ _ _ _ _ _user) -> do
        liftEff $ logShow $ e
        _ <- liftAff $ putVar _user varOrder
        pure TerminateEvent :: ReaderT _ (Web3 HttpProvider _) _

      void <<< event dxAddress $ \e@(DX.Cancel _ _ _ _ _ _ _user _ _ _) -> do
        liftEff $ logShow $ e
        _ <- liftAff $ putVar _user varCancel
        pure TerminateEvent :: ReaderT _ (Web3 HttpProvider _) _

      void <<< event dxAddress $ \e@(DX.Deposit _ _ _ _balance) -> do
        liftEff $ logShow $ e
        _ <- liftAff $ putVar _balance varDeposit
        pure TerminateEvent :: ReaderT _ (Web3 HttpProvider _) _

      event dxAddress $ \e@(DX.Withdraw _ _ _ _balance) -> do
        liftEff $ logShow $ e
        _ <- liftAff $ putVar _balance varWithdraw
        pure TerminateEvent :: ReaderT _ (Web3 HttpProvider _) _

    printVar varTrade
    printVar varOrder
    printVar varCancel
    printVar varDeposit
    printVar varWithdraw


-- printVar :: (Show a) => a -> Eff ()
printVar var = do
  val <- takeVar var
  liftEff $ log $ "Event: " <> show val
