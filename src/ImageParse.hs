module ImageParse where

import Codec.Picture
import qualified Data.Map as M

{-
Use parseImage to parse an RGB8 image into a nested list
of pixels (list of rows).

pixelToTuple is a helper function to convert
PixelRGB8 into an (R, G, B) tuple.

colTupleMap takes an RGB8 image and returns a nested list
of color tuples.

countryMap takes an RGB8 image and a map from color tuples to
country codes, and returns a nested list of country codes
in place of pixels, or the emptry string if the pixel
has a color not corresponding to a country.
-}
rowLooper img y x =
    if x == (imageWidth img) then [] else
        (pixelAt img x y) : (rowLooper img y (x + 1))

parseImage img =
    map (parseImgRow img)
        [0..((imageHeight img) - 1)]
    where
        parseImgRow img y =
            rowLooper img y 0

pixelToTuple (PixelRGB8 r g b) = (r, g, b)

colTupleMap img =
    map
        (\row -> (map (\px -> (pixelToTuple px)) row))
        (parseImage img)

codeLookup colToCode t =
    case (M.lookup t colToCode) of
            Just str -> str
            Nothing -> ""

countryMap img colToCode =
    map
        (\row -> (map (\t -> (find t)) row))
        (colTupleMap img)
    where
        find = codeLookup colToCode



{-
consumeListToFreqMap takes a map of keys X to integers
and a list, and adds the frequency of each X
in the list to the current value it maps to
(i.e. builds a frequency map one list at a time).

frequencyMap takes a nested list (such as what you
get from countryMap or colTupleMap) and returns
a map (tuples / country codes respectively in the
above cases) from elements to counts.
-}

consumeElementToFreqMap mp el =
    case (M.lookup el mp) of
        Just n -> (M.insert el (n + 1) mp)
        Nothing -> (M.insert el 1 mp)

consumeListToFreqMap mp l =
    foldl
        consumeElementToFreqMap
        M.empty
        l

frequencyMap nestedList =
    consumeListToFreqMap M.empty (concat nestedList)

nestedListElements nestedList =
    M.keys (frequencyMap nestedList)
    -- If changing this: returning keys in ascending order is important!
    -- (assumed by countryMapToColorMap)
