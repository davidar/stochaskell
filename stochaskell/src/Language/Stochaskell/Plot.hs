{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-|
Description : plotting utilities
Copyright   : (c) David A Roberts, 2015-2019
License     : GPL-3
Maintainer  : d@vidr.cc
Stability   : experimental
-}
module Language.Stochaskell.Plot
  ( ToPNG(..), kdeplot, renderAxis2
  -- * Re-exports
  -- ** "Graphics.Rendering.Chart.Easy"
  , module Graphics.Rendering.Chart.Easy
  , module Graphics.Rendering.Chart.Grid
  , module Graphics.Rendering.Chart.Plot.Histogram
  -- ** "Plots"
  , module Plots
  ) where

import Control.Monad.State
import Data.Monoid
import qualified Data.Vector.Generic as V
import Data.Vector (Vector)
import Diagrams.Backend.Cairo
import qualified Diagrams.Core
import qualified Diagrams.Path
import Diagrams.TwoD
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy hiding (
  (...),Plot,AxisStyle,Legend,Vector,beside,magma)
import Graphics.Rendering.Chart.Grid
import Graphics.Rendering.Chart.Plot.Histogram
import qualified Graphics.Rendering.Chart.Renderable
import Plots hiding (Plot,AxisStyle,Legend,magma)
import Statistics.Sample.KernelDensity.Simple

class ToPNG a where
  toPNG :: String -> a -> IO ()

instance ToPNG (Graphics.Rendering.Chart.Renderable.Renderable a) where
  toPNG f r = do
    _ <- renderableToFile def (f ++".png") r
    return ()

instance ToPNG (Diagrams.Core.QDiagram Cairo V2 Double Any) where
  toPNG f = renderCairo (f ++".png") $ mkSizeSpec2D (Just 800) (Just 600)

kdeplot :: String -> Double -> [Double] -> EC (Layout Double Double) ()
kdeplot s bw vals = plot $ line s [V.toList (fromPoints x) `zip` V.toList y]
  where dat = V.fromList vals :: Vector Double
        x = choosePoints 256 (bw * 3) dat
        y = estimatePDF gaussianKernel bw dat x

renderAxis2 :: State (Axis Cairo V2 Double) ()
            -> Diagrams.Core.QDiagram Cairo V2 Double Any
renderAxis2 = renderAxis . flip execState r2Axis
