{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Snake

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )

-- Types

-- | Ticks mark passing of time
--
-- | This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- | Named resources
--
-- | Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

data Cell = Snake | Food | Empty

-- | App definition

app :: App Game Tick Name
app = App { appDraw         = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = const theMap
          }

main :: IO ()
main = undefined


-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent = undefined

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI = undefined

theMap :: AttrMap
theMap = undefined

