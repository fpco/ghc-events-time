{-# LANGUAGE ScopedTypeVariables #-}

module GHC.Events.Time.Diagrams
  ( doubleHistogramDiagram
  , xyDiagram
  ) where


import qualified Control.Foldl as F
import           Data.Default.Class (def)
import           Data.Histogram (asList)
import           Data.Histogram.Fill
import           Data.Histogram.Generic (Histogram)
import qualified Data.Vector.Unboxed as U
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude hiding (sample, render, (<>))
import           Graphics.Rendering.Chart hiding (label)
import           Graphics.Rendering.Chart.Backend.Diagrams (DEnv, runBackend)
import           Text.Printf (printf)


numBins :: Int
numBins = 40


stats :: (F.Foldable f, Fractional a) =>
         f a -> (a, a, a)
stats v = F.fold stats v
  where
    stats = f <$> (F.premap (\x -> x * x) F.sum) <*> F.sum <*> F.genericLength
    f x2Sum xSum n = (var, mean, n)
      where
        mean = xSum / n
        mean2 = x2Sum / n
        var = n * (mean2 - mean * mean) / (n - 1)


hb :: F.Foldable f =>
      f Double -> HBuilder Double (Histogram U.Vector BinD Double)
hb xs = forceDouble -<< mkSimple (binD lower numBins upper)
  where
    (varX, xBar, _) = stats xs
    lower = xBar - 2.0 * sqrt varX
    upper = xBar + 2.0 * sqrt varX


hist :: F.Foldable f =>
        f Double -> Histogram U.Vector BinD Double
hist xs = fillBuilder (hb xs) xs


barChart :: String ->
            [(Double, Double)] ->
            Graphics.Rendering.Chart.Renderable ()
barChart title bvs = toRenderable layout
  where
    layout =
      layout_title .~ title
      $ layout_x_axis . laxis_generate .~ autoIndexAxis (map (printf "%4.3f" . fst) bvs)

      $ layout_y_axis . laxis_title .~ "Frequency"
      $ layout_plots .~ (map plotBars $ [bars1])
      $ def

    bars1 =
      plot_bars_titles .~ [title]
      $ plot_bars_values .~ addIndexes (map return $ map snd bvs)
      $ plot_bars_style .~ BarsClustered
      $ plot_bars_item_styles .~ [(solidFillStyle (blue `withOpacity` 0.25), Nothing)]
      $ def


barDiag :: DEnv ->
           String ->
           [(Double, Double)] ->
           Diagram B R2
barDiag denv title bvs = fst $ runBackend denv (render (barChart title bvs) (500, 500))


doubleHistogramDiagram :: DEnv -> String -> [Double] -> Diagram B R2
doubleHistogramDiagram denv label ds = barDiag denv label (asList (hist ds))


xyDiagram ::
  forall x y .
  (PlotValue x, PlotValue y) =>
  DEnv ->
  String ->
  (String, String) ->
  [(x, y)] ->
  Diagram B R2
xyDiagram denv title (xAxisTitle, yAxisTitle) durations =
  fst $ runBackend denv (render renderable (500, 500))

  where
    renderable :: Graphics.Rendering.Chart.Renderable ()
    renderable = toRenderable layout

    layout =
      layout_title .~ title
      $ layout_x_axis . laxis_title .~ xAxisTitle
      $ layout_y_axis . laxis_title .~ yAxisTitle
      $ layout_plots .~ [toPlot plotPoints]
      $ def

    plotPoints :: PlotPoints x y
    plotPoints =
      plot_points_title .~ title
      $ plot_points_values .~ durations
      $ plot_points_style %~ (point_color .~ (blue `withOpacity` 0.75))
      $ def
