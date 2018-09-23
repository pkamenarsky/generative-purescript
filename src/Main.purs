module Main where

import Prelude

import Color
import Color.Scale (sample)
import Color.Scale.Perceptual (magma)
import Data.Array
import Data.Foldable (fold)
import Data.Traversable
import Data.Int (toNumber)
import Data.Maybe
import Effect (Effect)
import Effect.Random
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Graphics.Drawing
import Math (sin, cos, pi)
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = do
    mcanvas <- getCanvasElementById "canvas"
    let canvas = unsafePartial (fromJust mcanvas)
    ctx <- getContext2D canvas
    pure unit

    deform <- sequence $ replicate length $ randomRange 2.0 5.0

    render ctx $
      translate 100.0 100.0 $ fold $ do
        r <- 0..10
        pure $ translate 0.0 (toNumber r * 20.0) $
          fold (lines deform)
  where
    length = 300

    lines :: Array Number -> Array Drawing
    lines deform = do
      y <- 10..30
      pure $ outlined (outlineColor (unsafePartial $ fromJust $ fromHexString "#000000") <> lineWidth (1.0 + (5.0 - ((toNumber y - 10.0) / 20.0) * 5.0))) $ path $ do
        x <- 0..(length - 1)
        let dy = unsafePartial $ fromJust $ deform !! x
        pure { x: toNumber x * 5.0
             , y: toNumber y * 10.0 + 0.5 + dy * ((toNumber y - 2.0) * 0.2 + 2.0)
             }
