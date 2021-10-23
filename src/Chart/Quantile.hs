{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Quantile chart patterns.
module Chart.Quantile
  (
    blendMidLineStyles,

    -- * chart patterns
    quantileChart,
    histChart,
    quantileHistChart,
    digitSurfaceChart,
  )
where

import Chart
import Control.Lens
import Data.Bifunctor
import Data.Foldable
import Data.Maybe
import Data.Text (Text)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty)
import Prelude hiding (abs)
import qualified Data.HashMap.Strict as HashMap

-- | /blendMidLineStyle n w/ produces n lines of size w interpolated between two colors.
--
-- These usually are used to represent probability bounds.
blendMidLineStyles :: Int -> Double -> (Colour, Colour) -> [LineStyle]
blendMidLineStyles l w (c1, c2) = lo
  where
    m = (fromIntegral l - 1) / 2 :: Double
    cs = (\x -> 1 - abs (fromIntegral x - m) / m) <$> [0 .. (l - 1)]
    bs = (\x -> blend x c1 c2) <$> cs
    lo = (\c -> defaultLineStyle & #size .~ w & #color .~ c) <$> bs

-- * charts
-- | Chart template for quantiles.
quantileChart ::
  Text ->
  [Text] ->
  [LineStyle] ->
  [AxisOptions] ->
  [NonEmpty Double] ->
  ChartSvg
quantileChart title' names ls as xs =
  mempty & #hudOptions .~ hudOptions' & #chartTree .~ chart'
  where
    hudOptions' =
      defaultHudOptions
        & #hudTitles .~ [defaultTitle title']
        & ( #hudLegend
              .~ Just
                ( defaultLegendOptions
                    & #ltext . #size .~ 0.1
                    & #vgap .~ 0.05
                    & #innerPad .~ 0.2
                    & #lplace .~ PlaceRight,
                  first LineA <$> zip ls names
                )
          )
        & #hudAxes .~ as

    chart' =
      zipWith (\s d -> LineChart s [d])
        ls
        (NonEmpty.zipWith Point [0 ..] <$> xs)

-- | histogram chart
histChart ::
  Text ->
  Maybe (NonEmpty Text) ->
  Range Double ->
  Int ->
  NonEmpty Double ->
  ChartSvg
histChart title' names r g xs =
  barChart defaultBarOptions barData'
    & (#hudOptions . #hudTitles .~ [defaultTitle title'])
  where
    barData' = BarData [hr] names Nothing
    hcuts = grid OuterPos r g
    h = fill hcuts xs
    hr = NonEmpty.fromList $
      (\(Rect x x' _ _) -> (x + x') / 2)
        <$> makeRects (IncludeOvers (width r / fromIntegral g)) h

-- | a chart drawing a histogram based on quantile information
--
-- Will error if less than 2 quantiles
quantileHistChart ::
  Text ->
  Maybe (NonEmpty Text) ->
  -- | quantiles
  NonEmpty Double ->
  -- | quantile values
  NonEmpty Double ->
  ChartSvg
quantileHistChart title' names qs vs =
  mempty & #hudOptions .~ hudOptions' & #chartTree .~ [chart']
  where
    hudOptions' =
      defaultHudOptions
        & #hudTitles
        .~ [defaultTitle title']
        & #hudAxes
        .~ [ maybe
               ( defaultAxisOptions & #axisTick . #tstyle
                   .~ TickRound (FormatPrec (Just 3)) 8 TickExtend
               )
               ( \x ->
                   defaultAxisOptions & #axisTick . #tstyle
                     .~ TickPlaced (zip (toList vs) x)
               )
               (toList <$> names)
           ]
    chart' = RectChart defaultRectStyle hr
    hr =
      NonEmpty.zipWith
        (\(y, w) (x, z) -> Rect x z 0 ((w - y) / (z - x)))
        (NonEmpty.zip qs (NonEmpty.fromList $ NonEmpty.drop 1 qs))
        (NonEmpty.zip vs (NonEmpty.fromList $ NonEmpty.drop 1 vs))

-- | pixel chart of digitized vs digitized counts
digitSurfaceChart ::
  SurfaceStyle ->
  SurfaceLegendOptions ->
  (Text, Text, Text) ->
  [Text] ->
  [(Int, Int)] ->
  [Chart Double]
digitSurfaceChart pixelStyle plo ts names ps =
  runHud (aspect 1) (hs0 <> hs1) (cs0 <> cs1)
  where
    l = length names
    pts = Point l l
    gr :: Rect Double
    gr = fromIntegral <$> Rect 0 l 0 l
    mapCount = foldl' (\m x -> HashMap.insertWith (+) x 1.0 m) HashMap.empty ps
    f :: Point Double -> Double
    f (Point x y) = fromMaybe 0 $ HashMap.lookup (floor x, floor y) mapCount
    (hs0, cs0) = makeHud gr (qvqHud ts names)
    (cs1, hs1) =
      surfacefl
        f
        (SurfaceOptions pixelStyle pts gr)
        plo

-- style helpers
qvqHud :: (Text, Text, Text) -> [Text] -> HudOptions
qvqHud ts labels =
  defaultHudOptions
    & #hudTitles .~ makeTitles ts
    & #hudAxes
      .~ [ defaultAxisOptions
             & #axisTick . #tstyle .~ TickPlaced (zip ((0.5 +) <$> [0 ..]) labels)
             & #place .~ PlaceLeft,
           defaultAxisOptions
             & #axisTick . #tstyle .~ TickPlaced (zip ((0.5 +) <$> [0 ..]) labels)
             & #place .~ PlaceBottom
         ]

makeTitles :: (Text, Text, Text) -> [Title]
makeTitles (t, xt, yt) =
  reverse
    [ defaultTitle t,
      defaultTitle xt & #place .~ PlaceBottom & #style . #size .~ 0.06,
      defaultTitle yt & #place .~ PlaceLeft & #style . #size .~ 0.06
    ]

