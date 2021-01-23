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
	if x == -1 then [] else
	(pixelAt img x y) : (rowLooper img y (x - 1))

parseImage img =
	map (parseImgRow img)
		[0..((imageHeight img) - 1)]
	where
		parseImgRow img y =
			rowLooper img y ((imageWidth img) - 1)

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
