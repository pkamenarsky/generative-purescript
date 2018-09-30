module Main where

import Color
import Data.Array
import Data.Maybe
import Data.Traversable
import Effect.Random
import Graphics.Drawing
import Prelude

import Color.Scale (sample)
import Color.Scale.Perceptual (magma)
import Data.Foldable (fold)
import Data.Int (toNumber)
import Effect (Effect)
import Graphics.Canvas (clearRect, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D)
import Math (sin, cos, pi)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

undefined :: ∀ a. a
undefined = unsafeCoerce unit

data Configuration = Configuration Int Int Number Number

fromPolar :: { r :: Number, rho :: Number } -> Point
fromPolar v = { x: v.r * cos v.rho, y: v.r * sin v. rho }

makePath :: Int -> Point -> Point -> Array Point
makePath n a b = do
  t <- range 0 n
  pure { x: toNumber t * dx + a.x, y: toNumber t * dy + a.y }
  where
    dx = (b.x - a.x) / toNumber n
    dy = (b.y - a.y) / toNumber n

initialiseLines :: Configuration -> Array (Array Point)
initialiseLines (Configuration n segments r1 r2) = do
  x <- range 1 n
  let x' = toNumber x / 48.0 * 360.0
  pure $ makePath segments (fromPolar { r: r1, rho: x' }) (fromPolar { r: r2 * 3.0, rho: x' })

random :: Effect Number
random = randomRange (-3.0) 3.0

randomPoint :: Effect Point
randomPoint = do
  x <- random
  y <- random
  pure { x, y }

randomArray :: ∀ a. Int -> Effect a -> Effect (Array a)
randomArray n gen = sequence $ replicate n gen

drawing :: Configuration -> Array (Array Point) -> Array (Array Point)
drawing cfg crawl = shepherdValues
  where
    lines = initialiseLines cfg
    shepherdLines = scanl (\a b -> { x: a.x + b.x, y: a.y + b.y }) { x: 0.0, y: 0.0 }
    shepherdValues = scanl (zipWith (\a b -> { x: a.x + b.x, y: a.y + b.y })) (replicate 100 { x: 0.0, y: 0.0 }) do
      line <- crawl
      pure $ shepherdLines line

main :: Effect Unit
main = do
    mcanvas <- getCanvasElementById "canvas"
    let canvas = unsafePartial (fromJust mcanvas)
    ctx <- getContext2D canvas
    pure unit

    width <- getCanvasWidth canvas
    height <- getCanvasHeight canvas

    crawl <- randomArray 128 (randomArray 128 randomPoint)

    clearRect ctx { x: 0.0, y: 0.0, width, height }

    render ctx $
      translate 500.0 500.0 $ do
        fold (lines crawl)
  where
    lines crawl = do
      -- line <- initialiseLines (Configuration 128 128 25.0 60.0)
      line <- drawing (Configuration 128 128 25.0 60.0) crawl
      pure $ outlined (outlineColor (unsafePartial $ fromJust $ fromHexString "#000000")) $ path line
