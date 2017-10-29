{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (void, forever)
import Control.Concurrent (threadDelay, forkIO)
import Data.Monoid
import qualified Graphics.Vty as V
import           Control.Monad.IO.Class           (liftIO)

import Brick.Types
import Brick.Widgets.Core

import qualified Reflex as R
import qualified Reflex.Host.App as RH
import           Brick.ReflexMain
import           Graphics.Vty(defAttr)
import           Brick.AttrMap(attrMap)



drawUI :: Maybe V.Event -> Int -> [Widget ()]
drawUI lastEvent count = [a]
 where
  a =
    (str $ "Last Vty event: " <> (show lastEvent))
      <=> (str $ "Counter value is: " <> (show count))


main :: forall t . t ~ R.SpiderTimeline R.Global => IO ()
main = R.runSpiderHost $ RH.hostApp $ mdo

  (counterE, counterT) <- RH.newExternalEvent

  _                    <- RH.performPostBuild $ do
    void $ liftIO $ forkIO $ forever $ do
      _ <- counterT ()
      threadDelay 1000000

  (eventE, finE, _) <- brickWrapper
    shouldHaltE
    (drawUI <$> lastEvent <*> counter)
    (pure $ const Nothing)
    (pure $ attrMap defAttr [])

  RH.performPostBuild_ $ do
    pure $ RH.infoQuit $ pure finE

  counter   <- R.count counterE
  lastEvent <- R.holdDyn Nothing eventE

  let shouldHaltE = R.fforMaybe eventE $ (=<<) $ \case
        V.EvKey V.KEsc _ -> Just ()
        _                -> Nothing

  pure ()
    
