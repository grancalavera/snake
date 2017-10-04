-- |  With FlexibleContexts enabled
--    you can have any type inside a typeclass.

{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad (guard)
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (.~), (%~), (^.))

-- | &    reverse application operator
-- | .~   set
-- | %~   over
-- | ^.   view

main = do
  putStrLn "Nothing"

data Person = Person
  { _name :: String
  , _email :: String
  , _address :: [String]
  } deriving (Show)

makeLenses ''Person

arturo = Person
  { _name     = "Arturo Pex"
  , _email    = "arturopex@gmail.com"
  , _address  = [ "1 Random Road"
                , "Suit 7"
                ]
  }
