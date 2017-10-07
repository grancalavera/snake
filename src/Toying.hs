{-# LANGUAGE TemplateHaskell #-}
module Toying where

import Control.Applicative ((<|>))
import Control.Monad (guard, forever, void, liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)
import Data.Char (toUpper)

import Data.Sequence (Seq,ViewL(..), ViewR(..), (<|))
import qualified Data.Sequence as S
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (.~), (%~), (^.))
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen)

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
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import qualified Data.Sequence as S

-- | &    reverse application operator
-- | .~   set
-- | %~   over
-- | ^.   view

toyingMain = do
  putStrLn "Nothing"

data Person = Person
  { _name :: String
  , _email :: Maybe String
  , _address :: [String]
  } deriving (Show)

makeLenses ''Person

arturo = Person
  { _name     = "Arturo Pex"
  , _email    = Just "arturopex@gmail.com"
  , _address  = [ "1 Random Road"
                , "Suit 7"
                ]
  }

jimmyh = Person
  { _name     = "Jimmy Henry"
  , _email    = Nothing
  , _address  = [ "62 Endwell Road"
                , "First floor flat"
                , "Brockley"
                , "London"
                , "SE4 2ND"
                ]
  }

-- MonadPlus
-- https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus#MonadPlus

a = fromMaybe "Fallback" (guard False >> Nothing)
b = fromMaybe "Fallback" (guard False >> Just "value")
c = fromMaybe "Fallback" (guard True >> Nothing)
d = fromMaybe "Fallback" (guard True >> Just "value")

e = putStrLn $ show [a, b, c, d]

-- most of the functions like nextFood have the same type:
-- f :: Game -> Game
-- they take the whole state of the game, perform one specific task
-- and then return a whole new state of the game

-- some of them have the type:
-- f :: Game -> Maybe Game
-- and those ones involve chance, luck or something that might fail
-- somehow. for instance `die`, where the player might not die: if
-- the player doesn't die, then `die :: Game -> Maybe Game` will
-- return `Nothing`

-- is not exactly what I was thinking about but
-- is illustrative of how to use `over`
-- (I'm leaving the person hanging because I want to and I can)
yellAddress :: Person -> Person
yellAddress = address %~ (liftM $ map toUpper)
