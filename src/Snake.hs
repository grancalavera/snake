{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Snake where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Data.Sequence (Seq,ViewL(..), ViewR(..), (<|))

