module ImageGen where

import Codec.Picture
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Vector as V
import ImageParse
import System.Random

{-
To get O(1) lookup in the nested list, we need to convert it to
a nested vector first. netsedListToVector does this.

The default color scheme is that countries get randomly assigned
colors. The current random color logic is very primitive and
to be replaced with something fancier.

countryMapToColorMap takes a nested list representing a map
where each pixel position has a country code, generates a
"random" pairing of country codes and colors, and then maps
a (code) -> (color tuple) -> (pixel of that color)
function over everything.

getImageFromList should take a nested list like the output
of countryMapToColorMap and convert into an Image.
-}

nestedListToVector nl = 
    V.fromList (map V.fromList nl)


nRandomRGBVals seed n =
    take n $ randomRs (0, 255) (mkStdGen seed)
    -- fix: do something else instead of mkStdGen

pairListsIntoTriples [] [] [] = []
pairListsIntoTriples (a:as) (b:bs) (c:cs) =
    (a, b, c) : (pairListsIntoTriples as bs cs)

nRandomColTuples n =
    pairListsIntoTriples
        (nRandomRGBVals 0 n)
        (nRandomRGBVals 1 n)
        (nRandomRGBVals 2 n)

colorTupleMap colors els =
    randomColorMapper colors els M.empty
    where
        randomColorMapper colors els mp = if els == [] then mp else
            randomColorMapper
                (tail colors)
                (tail els)
                (M.insert (head els) (head colors) mp)

countryMapToColorMap cmap =
    map
        (\row -> (map
            (\ccode -> case (M.lookup ccode colorsMap) of
                Just (r, g, b) -> PixelRGB8 r g b
                Nothing -> PixelRGB8 0 0 0)
            row))
        cmap
    where
        colorsMap =
            colorTupleMap (nRandomColTuples 256) (nestedListElements cmap) 

getImageFromList nlMap =
    generateImage
        (\x -> \y -> (vmap V.! y) V.! x)
        (V.length (vmap V.! 0))
        (V.length vmap)
    where
        vmap = nestedListToVector nlMap