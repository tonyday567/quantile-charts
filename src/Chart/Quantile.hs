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
    quantileHistChart,
    digitSurfaceChart,
  )
where

import Chart
import Optics.Core
import Data.Foldable
import Data.Maybe
import Data.Text (Text)
import Prelude hiding (abs)
import qualified Data.HashMap.Strict as HashMap

-- * charts
-- | Chart template for quantiles.
quantileChart ::
  Text ->
  [Text] ->
  [LineStyle] ->
  [AxisOptions] ->
  [[Double]] ->
  ChartSvg
quantileChart title' names ls as xs =
  mempty & #charts .~ chart0 & #hudOptions .~ hudOptions'
  where
    hudOptions' :: HudOptions
    hudOptions' =
      defaultHudOptions
        & #titles .~ [(10,defaultTitle title')]
        & ( #legends
              .~
                [(12, defaultLegendOptions
                    & #textStyle % #size .~ 0.1
                    & #vgap .~ 0.05
                    & #innerPad .~ 0.2
                    & #place .~ PlaceRight
                    & #content .~ undefined -- zipWith (\(l,name) -> (LineChart l [[(Point 0 0, Point 1 1)]], name)) ls names
                )]
          )
        & set #axes ((5,) <$> as)

    chart0 = unnamed $
      zipWith (\s d -> LineChart s [d])
        ls
        (zipWith Point [0 ..] <$> xs)

-- | histogram chart
histChart ::
  Text ->
  [Text] ->
  Range Double ->
  Int ->
  [Double] ->
  ChartSvg
histChart title' names r g xs =
  barChart defaultBarOptions barData'
    & (#hudOptions % #titles .~ [(10,defaultTitle title')])
  where
    barData' = BarData [hr] names []
    hcuts = grid OuterPos r g
    h = fill hcuts xs
    hr =
      (\(Rect x x' _ _) -> (x + x') / 2)
        <$> makeRects (IncludeOvers (width r / fromIntegral g)) h

-- | a chart drawing a histogram based on quantile information
--
-- Will error if less than 2 quantiles
quantileHistChart ::
  Text ->
  Maybe [Text]->
  -- | quantiles
  [Double]->
  -- | quantile values
  [Double]->
  ChartSvg
quantileHistChart title' names qs vs =
  mempty & #hudOptions .~ hudOptions' & #charts .~ unnamed [chart0]
  where
    hudOptions' =
      defaultHudOptions
        & #titles
        .~ [(10, defaultTitle title')]
        & #axes
        .~ [ maybe
               (5, defaultAxisOptions & #ticks % #style
                   .~ TickRound (FormatPrec (Just 3)) 8 TickExtend)
               ( \x ->
                   (5, defaultAxisOptions & #ticks % #style
                     .~ TickPlaced (zip (toList vs) x))
               )
               (toList <$> names)
           ]
    chart0 = RectChart defaultRectStyle hr
    hr = zipWith
        (\(y, w) (x, z) -> Rect x z 0 ((w - y) / (z - x)))
        (zip qs (drop 1 qs))
        (zip vs (drop 1 vs))

-- | pixel chart of digitized vs digitized counts
digitSurfaceChart ::
  SurfaceStyle ->
  SurfaceLegendOptions ->
  (Text, Text, Text) ->
  [Text] ->
  [(Int, Int)] ->
  Charts (Maybe Text)
digitSurfaceChart pixelStyle plo ts names ps =
  runHud (aspect 1) (hs0 <> hs1) (unnamed [BlankChart [cs0]] <> unnamed cs1)
  where
    l = length names
    pts = Point l l
    gr :: Rect Double
    gr = fromIntegral <$> Rect 0 l 0 l
    mapCount = foldl' (\m x -> HashMap.insertWith (+) x 1.0 m) HashMap.empty ps
    f :: Point Double -> Double
    f (Point x y) = fromMaybe 0 $ HashMap.lookup (floor x, floor y) mapCount
    (hs0, cs0) = toHuds (qvqHud ts names) gr
    (cs1, hs1) =
      surfacefl
        f
        (SurfaceOptions pixelStyle pts gr)
        plo

-- style helpers
qvqHud :: (Text, Text, Text) -> [Text] -> HudOptions
qvqHud ts labels =
  defaultHudOptions
    & #titles .~ makeTitles 10 ts
    & #axes
      .~ ((10,) <$> [ defaultAxisOptions
             & #ticks % #style .~ TickPlaced (zip ((0.5 +) <$> [0 ..]) labels)
             & #place .~ PlaceLeft,
           defaultAxisOptions
             & #ticks % #style .~ TickPlaced (zip ((0.5 +) <$> [0 ..]) labels)
             & #place .~ PlaceBottom
         ])

makeTitles :: Priority -> (Text, Text, Text) -> [(Priority, Title)]
makeTitles p (t, xt, yt) =
  (p,) <$> reverse
    [ defaultTitle t,
      defaultTitle xt & #place .~ PlaceBottom & #style % #size .~ 0.06,
      defaultTitle yt & #place .~ PlaceLeft & #style % #size .~ 0.06
    ]

