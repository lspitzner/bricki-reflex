{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Monoid
import qualified Graphics.Vty as V


import qualified Reflex as R
import qualified Reflex.Host.App as RH
import           Brick.ReflexMain


import Brick.Types                         ( Widget )
import Brick.Widgets.Core                  ( vBox
                                           , str
                                           )
import           Graphics.Vty(defAttr)
import           Brick.AttrMap(attrMap)



drawUI :: String -> [Widget ()]
drawUI st = [ui]
 where
  ui = vBox
    [ str $ "External input: \"" <> st <> "\""
    , str "(Press Esc to quit or Space to ask for input)"
    ]


main :: forall t . t ~ R.SpiderTimeline R.Global => IO ()
main = R.runSpiderHost $ RH.hostApp $ mdo

  (eventE, finE, suspendCB) <- brickWrapper       shouldHaltE
      (drawUI <$> redrawDyn)
      (pure $ neverShowCursor ())
      (pure $ attrMap defAttr [])

  RH.performPostBuild_ $ do
    pure $ RH.infoQuit $ pure finE

  resultE <- suspendCB $ flip R.fmapMaybe eventE $ \case
    Just (V.EvKey (V.KChar ' ') []) -> Just $ do
      putStrLn "Suspended. Please enter something and press enter to resume:"
      getLine
    _ -> Nothing

  -- resultDyn <- R.holdDyn "" resultE
  -- t1 <- R.holdDyn Nothing eventE
  -- let redrawDyn = const <$> resultDyn <*> t1
  resultB   <- R.hold "" resultE
  redrawDyn <- R.holdDyn "" $ R.switch $ (<$eventE) <$> resultB 
  -- foo <- R.count $ R.tag resultB eventE

  let shouldHaltE = R.fforMaybe eventE $ (=<<) $ \case
        V.EvKey V.KEsc _ -> Just ()
        _                -> Nothing

  pure ()

