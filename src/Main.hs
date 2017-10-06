-- With FlexibleContexts enabled
-- you can have any type inside a typeclass.

{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad (guard)
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (.~), (%~), (^.))
import Data.Maybe (fromMaybe)

-- | &    reverse application operator
-- | .~   set
-- | %~   over
-- | ^.   view

main = do
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

-- all the functions like nextFood have the same type:
-- f :: Game -> Game
-- they take the whole state of the game, perform one specific task
-- and then return a whole new state of the game
