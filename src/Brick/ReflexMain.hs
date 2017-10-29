{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Brick.ReflexMain
  ( brickWrapper
  , module Brick.Main
    -- * same interface but without forcing the user to use mfix
  , brickWrapperNoFix
  )
where



import qualified Reflex as R
import qualified Reflex.Host.Class as R
import qualified Reflex.Host.App as RH

import           Brick.Types                      ( Widget
                                                  , locationRowL
                                                  , locationColumnL
                                                  , CursorLocation(..)
                                                  , Extent
                                                  )
import           Brick.Types.Internal             ( RenderState(..)
                                                  )
import           Brick.Widgets.Internal           ( renderFinal
                                                  )
import           Brick.AttrMap
import           Brick.Main                       ( neverShowCursor
                                                  , showFirstCursor
                                                  , showCursorNamed
                                                  )

import qualified Data.Map as M
import qualified Data.Set as S

import           Control.Monad
import           Data.Functor
import           Control.Concurrent
import           Control.Exception                (finally)
import           Data.Monoid
import           Lens.Micro                       ((^.), (<&>))
import           Data.Align
import           Data.These
import           Data.IORef
import           Control.Monad.IO.Class

import           Graphics.Vty
                                                  ( Vty
                                                  , Picture(..)
                                                  , Cursor(..)
                                                  , Event(..)
                                                  , update
                                                  , outputIface
                                                  , inputIface
                                                  , displayBounds
                                                  , shutdown
                                                  , mkVty
                                                  , defaultConfig
                                                  )
import           Graphics.Vty.Input               ( _eventChannel
                                                  )
import           Control.Concurrent.STM.TChan
import           Control.Monad.STM



-- | Main interface of the reflex interface to brick. This interface
-- is designed in such a way that you will most likely have to make use of
-- the MonadFix instance of 'AppHost' one way or another
-- (@mdo@\/@do rec@\/@mfix@), because one of the return values of
-- @brickWrapper@ is the input event that most likely determines the content
-- of the widgets to draw.
--
-- All \'Event\'s other than the one in \'Maybe Event\' are /reflex/
-- @Event@s.
-- The remaining one is brick's "input" event type.
--
-- Triggering any of the output 'Dynamic's causes a redraw.
-- 
-- For the suspending-functionality, the following caveats apply:
-- 
--     * Starting a second IO-action before all previously started ones
--       have "returned" leads is undefined behaviour (i.e. the current
--       implementation might not even 'error' out but fail in some
--       random other fashion). (If I tried to phrase this in terms of Events,
--       it would enlighten neither of us.)
--
--     * Sending a "halt" signal to this brick interface while this
--       is in suspended state will properly shut down the brick network
--       but it will not stop the IO-action itself. Should this scenario
--       become relevant, the user is free to killThread the action
--       manually (but bare in mind that the action will run in a forked
--       thread, so the action would have to pass out its ThreadId via
--       MVar or such as the first thing it does).
--
--     * It is possible but generally not recommended to use the suspension
--       callback function multiple times. If suspensions don't overlap it
--       will probably work, but it is not tested. Also, in theory we could
--       not bind the @a@ at top-level, i.e. make the callback have type
--       @forall a . Banana.Event (IO a) -> Banana.MomentIO (Banana.Event a)@.
--       If you ever find that useful, please open an issue.
--
--     * If the 'IO' action terminates via exception, the brick interface
--       will resume, but no \'result\' event will be triggered.
--
-- Apart from the documentation here, please also consider the documentation
-- of the non-reactive-banana brick interface.
brickWrapper
  :: forall n t a
   . (Ord n, R.ReflexHost t, MonadIO (R.PushM t), MonadIO (R.HostFrame t))
  => R.Event t ()
     -- ^ event to trigger shutdown
  -> R.Dynamic t [Widget n]
     -- ^ dynamic that contains the widget(s) to draw
  -> R.Dynamic t ([CursorLocation n] -> Maybe (CursorLocation n))
     -- ^ dynamic that contains the cursor selection function
  -> R.Dynamic t AttrMap
     -- ^ dynamic that contains the attribute-map
  -> RH.AppHost
       t
       ( R.Event t (Maybe Event)
       , R.Event t ()
       , R.Event t (IO a) -> RH.AppHost t (R.Event t a)
       )
     -- ^ returns three results:
     -- 1) A (reflex) event containing any (brick) input events
     -- 2) An event that fires once when shutdown of the brick interface is
     --    completed
     -- 3) a function that can be used to implement suspension of the brick
     --    ui to run some external commands etc. see the demo program for
     --    usage.
brickWrapper shouldHaltE widgetDyn cursorDyn attrDyn = do
  let initialRS = RS M.empty [] S.empty mempty []

  (eventEvent   , eventH   ) <- RH.newExternalEvent
  (shutdownEvent, shutdownH) <- RH.newExternalEvent
  startupEvent               <- RH.getPostBuild
  (restartEvent, restartH)   <- RH.newExternalEvent
  (suspendEvent, suspendH)   <- RH.newExternalEvent

  RH.performEvent_ $ (liftIO . void . forkIO) <$> suspendEvent

  let suspendSetup :: forall a . R.Event t (IO a) -> RH.AppHost t (R.Event t a)
      suspendSetup ioE = do
        (resultEvent, resultH) <- RH.newExternalEvent
        RH.performEvent_ $ ioE <&> \io ->
          liftIO
            $ void
            $ suspendH
            $ void
            $ ((io >>= resultH) `finally` restartH ())
        return resultEvent

  initStateDyn <- do
    let e1 = startupEvent <> restartEvent
        e2 = suspendEvent
    R.foldDynM id Nothing
      $   align e1 (R.leftmost [e2 $> (), shouldHaltE])
      <&> \case
            This{} -> \_ -> liftIO $ do
              vty <- liftIO $ do
                x <- mkVty defaultConfig
                return x
              let loop = forever $ do
                    ev <- atomically
                      (readTChan $ _eventChannel $ inputIface vty)
                    case ev of
                      (EvResize _ _) ->
                        eventH
                          .   Just
                          .   (\(w, h) -> EvResize w h)
                          =<< (displayBounds $ outputIface vty)
                      _ -> eventH $ Just ev
              pumpTId <- liftIO $ forkIO $ loop
              void $ forkIO $ void $ eventH $ Nothing
              let stopper = do
                    killThread pumpTId
                    shutdown vty
              return $ pure (vty, stopper)
            That{} -> \mState -> do
              liftIO $ mState `forM_` snd
              return Nothing
            These{} ->
              error "brick internal error: simultaneous startup/suspend"

  -- using push does not work. Don't know why. Probably not supposed to work.
  -- rec renderStateB <- R.hold initialRS renderStateE
  --     let renderStateE = R.push id $ R.updated
  --           [ do
  --             mState <- R.sample $ R.current initStateDyn
  --             renderState <- R.sample renderStateB
  --             case mState of
  --               Nothing       -> pure Nothing
  --               Just (vty, _) -> liftIO $ fmap Just $ render vty
  --                                                            widgetStack
  --                                                            chooseCursor
  --                                                            attrs
  --                                                            renderState
  --           | widgetStack  <- widgetDyn
  --           , chooseCursor <- cursorDyn
  --           , attrs        <- attrDyn
  --           ]
  -- 
  -- return ()

  rsRef <- liftIO $ newIORef initialRS

  RH.performEvent_ $ shouldHaltE <&> \() -> liftIO $ void $ shutdownH ()

  let refreshE :: R.Event t (R.HostFrame t ()) = R.updated
        [ do
            mState <- R.sample $ R.current initStateDyn
            case mState of
              Nothing       -> pure ()
              Just (vty, _) -> liftIO $ do
                renderState           <- readIORef rsRef
                (renderState', _exts) <- render vty
                                                widgetStack
                                                chooseCursor
                                                attrs
                                                renderState
                writeIORef rsRef renderState'
        | widgetStack  <- widgetDyn
        , chooseCursor <- cursorDyn
        , attrs        <- attrDyn
        ]

  RH.performEvent_ $ refreshE `R.difference` shouldHaltE

  pure (eventEvent, shutdownEvent, suspendSetup)



-- | Firstly, it is strongly recommended to use 'brickWrapper' instead, as
-- it exposes exactly the same functionality.
--
-- I'll just put a well-formatted, simplified (remove @t@) version of the
-- type signature
-- here, which makes this prone to become
-- out of date (yay..) but allows me to properly tag everything (yay!)
--
-- > brickWrapperNoFix
-- >   :: forall n
-- >    . ( Ord n
-- >      )
-- >   => (  R.Event (Maybe Event)                     -- s1: brick event source
-- >      -> R.Event ()                                -- s2: post shutdown event
-- >      -> ( forall a . R.Event (IO a)
-- >           -> RH.AppHost (R.Event a)
-- >         )                                         -- s3: suspender with "callback"
-- >      -> RH.AppHost
-- >           t
-- >           ( R.Event ()                            -- o1: brick halt
-- >           , R.Dynamic [Widget n]                  -- o2: widget layers to draw
-- >           , R.Dynamic                             -- o3: cursor selection function
-- >               ([CursorLocation n] -> Maybe (CursorLocation n))
-- >           , R.Dynamic AttrMap                     -- o4: brick global attribute map
-- >           )
-- >      )
-- >   -> RH.AppHost ()
--
-- All \'Event\'s other than the one in __s1__ are /reflex/ @Event@s.
-- The lonely one is brick's "input" event type.
--
-- [s1]: Just Event or redraw-trigger
--
-- [s2]: Fires after shutdown of wrapper
--
-- [s3]:
--     Callback to register IO-actions to run while brick is suspended.
--     Results are returned in the result.
--
--     Note that:
--
--     a) Starting a second IO-action before all previously started ones
--        have "returned" leads is undefined behaviour (i.e. the current
--        implementation might not even 'error' out but fail in some
--        random other fashion). (If I tried to phrase this in terms of Events,
--        it would enlighten neither of us.)
--
--     b) Sending a "halt" signal to this brick interface while this
--        is in suspended state will properly shut down the brick network
--        but it will not stop the IO-action itself. Should this scenario
--        become relevant, the user is free to killThread the action
--        manually (but bare in mind that the action will run in a forked
--        thread, so the action would have to pass out its ThreadId via
--        MVar or such as the first thing it does).
--
-- [o1]: initiates shutdown of brick
--
-- [o2, o3, o4]:
--     as the short description says; see the non-brick interface for
--     details. Redraws happen whenever any of these three Dynamics change.

brickWrapperNoFix
  :: forall n t a
   . (Ord n, R.ReflexHost t, MonadIO (R.PushM t), MonadIO (R.HostFrame t))
  => (  R.Event t (Maybe Event)
     -> R.Event t ()
     -> (R.Event t (IO a) -> RH.AppHost t (R.Event t a))
     -> RH.AppHost
          t
          ( R.Event t ()
          , R.Dynamic t [Widget n]
          , R.Dynamic
              t
              ([CursorLocation n] -> Maybe (CursorLocation n))
          , R.Dynamic t AttrMap
          )
     ) -- ^ one line :/
  -> RH.AppHost t ()
brickWrapperNoFix interfaceF = mdo
  (shouldHaltE, widgetDyn, cursorDyn, attrDyn) <- interfaceF eventEvent
                                                             shutdownEvent
                                                             suspendSetup
  (eventEvent, shutdownEvent, suspendSetup) <- brickWrapper shouldHaltE
                                                            widgetDyn
                                                            cursorDyn
                                                            attrDyn
  pure ()


render
  :: Vty
  -> [Widget n]
  -> ([CursorLocation n] -> Maybe (CursorLocation n))
  -> AttrMap
  -> RenderState n
  -> IO (RenderState n, [Extent n])
render vty widgetStack chooseCursor attrMapCur rs = do
  sz <- displayBounds $ outputIface vty
  let (newRS, pic, theCursor, exts) =
        renderFinal attrMapCur widgetStack sz chooseCursor rs
      picWithCursor = case theCursor of
        Nothing  -> pic { picCursor = NoCursor }
        Just loc -> pic
          { picCursor = Cursor (loc ^. locationColumnL) (loc ^. locationRowL)
          }

  update vty picWithCursor

  return (newRS, exts)
