{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wall #-}

-- | Quantile chart patterns.
module Chart.Quantile
  (
    -- * quantile chart patterns
    quantileChart,
    histChart,

    -- * testing
    qua,
    qLines,
  )
where

import Chart
import Optics.Core
import Data.Foldable
import Data.Maybe
import Data.Text (Text)
import Prelude hiding (abs)
import Statistics.Distribution.Normal
import Statistics.Distribution
import Data.Bool

-- * charts

-- | Chart template for quantile data.
quantileChart ::
  [Text] ->
  [LineStyle] ->
  [AxisOptions] ->
  [[Double]] ->
  ChartSvg
quantileChart names ls as xs =
  mempty & #charts .~ chart0 & #hudOptions .~ hudOptions'
  where
    hudOptions' :: HudOptions
    hudOptions' =
      defaultHudOptions
        & ( #legends
              .~
                [(12, defaultLegendOptions
                    & #textStyle % #size .~ 0.1
                    & #vgap .~ 0.05
                    & #innerPad .~ 0.2
                    & #place .~ PlaceRight
                    & #content .~ zip names (fmap (\l -> LineChart l [[Point 0 0, Point 1 1]]) ls)
                )]
          )
        & set #axes ((5,) <$> as)

    chart0 = unnamed $
      zipWith (\s d -> LineChart s [d])
        ls
        (zipWith Point [0 ..] <$> xs)

-- | histogram chart
histChart ::
  Range Double ->
  Int ->
  [Double] ->
  ChartSvg
histChart r g xs =
  barChart defaultBarOptions barData'
  where
    barData' = BarData [freqs] xs'' []
    hcuts = grid OuterPos r g
    h = fill hcuts xs
    counts =
      (\(Rect _ _ _ w) -> w)
        <$> makeRects (IncludeOvers (width r / fromIntegral g)) h
    freqs = (/sum counts) <$> counts
    xs' =
      (\(Rect x x' _ _) -> (x + x') / 2)
        <$> makeRects (IncludeOvers (width r / fromIntegral g)) h
    xs'' = ["unders"] <> take (length xs' - 2) (drop 1 (comma (Just 2) <$> xs')) <> ["overs"]

qua :: Double -> Double -> Double -> Double -> Double
qua u s t p
  | t <= 0 = u*t
  | otherwise = Statistics.Distribution.quantile (normalDistr (u*t) (s*sqrt t)) p

qLines :: Double -> Int -> Colour -> [LineStyle]
qLines s n c = (\x -> defaultLineStyle & #color .~ x & #size .~ s) <$> cqs n c

cqs :: Int -> Colour -> [Colour]
cqs n c = fmap (\x -> mix x c (greyed c)) xs
  where
    xs = (\t -> fmap (\x -> fromIntegral (abs (x-(t `div` 2)-bool 0 1 (x>(t `div`2) && 1==t `mod` 2))) / fromIntegral (t `div` 2)) [0..t]) (n - 1)
