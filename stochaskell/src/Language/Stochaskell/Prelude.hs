{-|
Description : modified Prelude
Copyright   : (c) David A Roberts, 2015-2019
License     : GPL-3
Maintainer  : d@vidr.cc
Stability   : experimental
-}
module Language.Stochaskell.Prelude
  ( module Prelude
  , module Control.Monad
  , module Control.Monad.Guard
  , module Data.Boolean.Overload
  , module Data.List
  , module Data.Maybe
  , module Data.Monoid
  ) where

import Prelude hiding (
  (==),(/=),(<),(>),(<=),(>=),(<*),(*>),(&&),(||),
  max,min,not,foldl,foldr,scanl,scanr)
import Control.Monad hiding (guard)
import Control.Monad.Guard
import Data.Boolean.Overload hiding (boolean) -- TODO: no infix declaration
import Data.List hiding (foldl,foldr,scanl,scanr)
import Data.Maybe
import Data.Monoid
