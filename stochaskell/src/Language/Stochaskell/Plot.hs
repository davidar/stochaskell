{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-|
Description : plotting utilities
Copyright   : (c) David A Roberts, 2015-2019
License     : GPL-3
Maintainer  : d@vidr.cc
Stability   : experimental
-}
module Language.Stochaskell.Plot
  ( ToPNG(..), renderAxis2
  -- * Re-exports
  -- ** "Graphics.Rendering.Chart.Easy"
  , module Graphics.Rendering.Chart.Easy
  , module Graphics.Rendering.Chart.Plot.Histogram
  -- ** "Plots"
  , module Plots
  ) where

import Control.Monad.State
import Data.Monoid
import Diagrams.Backend.Cairo
import qualified Diagrams.Core
import qualified Diagrams.Path
import Diagrams.TwoD
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy hiding ((...),Plot,AxisStyle,Legend,magma)
import Graphics.Rendering.Chart.Plot.Histogram
import qualified Graphics.Rendering.Chart.Renderable
import Plots hiding (Plot,AxisStyle,Legend,magma)

class ToPNG a where
  toPNG :: String -> a -> IO ()

instance ToPNG (Graphics.Rendering.Chart.Renderable.Renderable a) where
  toPNG f r = do
    _ <- renderableToFile def (f ++".png") r
    return ()

instance ToPNG (Diagrams.Core.QDiagram Cairo V2 Double Any) where
  toPNG f = renderCairo (f ++".png") $ mkSizeSpec2D (Just 800) (Just 600)

renderAxis2 :: State (Axis Cairo V2 Double) ()
            -> Diagrams.Core.QDiagram Cairo V2 Double Any
renderAxis2 = renderAxis . flip execState r2Axis
