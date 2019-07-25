{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-|
Description : plotting utilities
Copyright   : (c) David A Roberts, 2015-2019
License     : GPL-3
Maintainer  : d@vidr.cc
Stability   : experimental
-}
module Language.Stochaskell.Plot
  ( PlotP(..), ToPNG(..)
  , kde, kde', kdeplot, kdeplot'
  , plotUnder
  , renderAxis2
  , xlabel, xlim, ylabel, ylim
  -- * Re-exports
  -- ** "Graphics.Rendering.Chart.Easy"
  , module Graphics.Rendering.Chart.Easy
  , module Graphics.Rendering.Chart.Grid
  , module Graphics.Rendering.Chart.Plot.FillBetween
  , module Graphics.Rendering.Chart.Plot.Histogram
  -- ** "Plots"
  , module Plots
  ) where

import Prelude ()
import Language.Stochaskell hiding (Vector)

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
  (...),Plot,AxisStyle,Legend,Vector,beside,magma,tan)
import Graphics.Rendering.Chart.Grid
import Graphics.Rendering.Chart.Plot.FillBetween
import Graphics.Rendering.Chart.Plot.Histogram
import qualified Graphics.Rendering.Chart.Renderable
import Plots hiding (Plot,AxisStyle,Legend,magma,tan)
import qualified Statistics.Sample.KernelDensity as KD
import Statistics.Sample.KernelDensity.Simple

class PlotP t where
  plotP :: P (Expression t) -> Int -> String -> IO (EC (Layout Double Double) ())
  -- | restricted to positive domain
  plotP' :: P (Expression t) -> Int -> String -> IO (EC (Layout Double Double) ())
  plotP' = plotP

instance PlotP Integer where
  plotP p n title = do
    samples <- sequence [simulate p | i <- [1..n]]
    let a = minimum samples
        b = maximum samples
    return $ do
      col <- takeColor
      plot . return . histToPlot $ defaultNormedPlotHist
        { _plot_hist_title = title
        , _plot_hist_values = integer <$> samples
        , _plot_hist_range = Just (integer a, integer b)
        , _plot_hist_bins = integer (b-a)
        , _plot_hist_fill_style = FillStyleSolid $ dissolve 0.1 col
        , _plot_hist_line_style = defaultPlotLineStyle { _line_color = col }
        }

instance PlotP Double where
  plotP p n title = do
    samples <- sequence [simulate p | i <- [1..n]]
    let vals = real <$> samples
        support = maximum vals - minimum vals
    return $ plotUnder title [(x,y) | (x,y) <- kde' vals, y > 0.01 / support]

  plotP' p n title = do
    samples <- sequence [simulate p | i <- [1..n]]
    let support = real (maximum samples)
        vals = log . real <$> samples
    return $ plotUnder title [(x', y') | (x,y) <- kde' vals
                                       , let x' = exp x, let y' = y / x'
                                       , y' > 0.01 / support , y' < 10 / support]

class ToPNG a where
  toPNG :: String -> a -> IO ()

instance ToPNG (Graphics.Rendering.Chart.Renderable.Renderable a) where
  toPNG f r = do
    _ <- renderableToFile def (f ++".png") r
    return ()

instance ToPNG (Diagrams.Core.QDiagram Cairo V2 Double Any) where
  toPNG f = renderCairo (f ++".png") $ mkSizeSpec2D (Just 800) (Just 600)

kdeplot :: String -> Double -> [Double] -> EC (Layout Double Double) ()
kdeplot s bw vals = plot $ line s [kde bw vals]

kdeplot' :: String -> [Double] -> EC (Layout Double Double) ()
kdeplot' s vals = plot $ line s [kde' vals]

kde :: Double -> [Double] -> [(Double,Double)]
kde bw vals = V.toList (fromPoints x) `zip` V.toList y
  where dat = V.fromList vals :: Vector Double
        x = choosePoints 256 (bw * 3) dat
        y = estimatePDF gaussianKernel bw dat x

kde' :: [Double] -> [(Double,Double)]
kde' vals = V.toList x `zip` V.toList y
  where (x,y) = KD.kde 256 (V.fromList vals :: Vector Double)

plotUnder :: String -> [(Double,Double)] -> EC (Layout Double Double) ()
plotUnder title values = do
  col <- takeColor
  plot . liftEC $ do
    plot_fillbetween_style .= solidFillStyle (dissolve 0.1 col)
    plot_fillbetween_values .= [(x,(0,y)) | (x,y) <- values]
  plot . liftEC $ do
    plot_lines_title .= title
    plot_lines_values .= [values]
    plot_lines_style . line_color .= col

renderAxis2 :: State (Axis Cairo V2 Double) ()
            -> Diagrams.Core.QDiagram Cairo V2 Double Any
renderAxis2 = renderAxis . flip execState r2Axis

xlim l = layout_x_axis . laxis_generate .= scaledAxis def l
ylim l = layout_y_axis . laxis_generate .= scaledAxis def l
xlabel s = layout_x_axis . laxis_title .= s
ylabel s = layout_y_axis . laxis_title .= s
