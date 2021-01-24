module ProcessMap where

import Codec.Picture
import ImageParse
import ImageGen
import System.Random
import qualified Data.Map as M

transposeNestedList nl =
    if null (head nl) then [] else
        (map head nl) : (transposeNestedList (map tail nl))

padBy paddingEl 0 l = l
padBy paddingEl n l = paddingEl : (padBy paddingEl (n - 1) l)

nlPadder paddingEl [] [] = []
nlPadder paddingEl (toAdd:addNums) (l:ls) =
    (padBy paddingEl toAdd l) : (nlPadder paddingEl addNums ls)

{-
padNestedList takes a list of lists nl and adds paddingEl
to the front of each list until the length of each list in the list
is the maximum length of any list in the list.
-}
padNestedList paddingEl nl =
    let lengths = map length nl in
    let maxlen = maximum lengths in
    let deltas = map (\len -> maxlen - len) lengths in
    map reverse (nlPadder paddingEl deltas (map reverse nl))


nRandFloats seed n =
    take n $ randoms (mkStdGen seed) :: [Float]

randFloatsMap seed cmap = 
    let rowLength = length (head cmap) in
    map (const $ nRandFloats seed rowLength) [1..(length cmap)]

{-
scaleFactorsMap get a map where instead of country codes in each
point you have the scale factor by which that country has to
be scaled.
-}
scaleFactorsMap scaleFactors cmap =
    map (\row ->
        (map (\ccode -> (M.findWithDefault 1 ccode scaleFactors))
            row))
        cmap

nestedZipper [] [] = []
nestedZipper (a:as) (b:bs) = (zip a b) : (nestedZipper as bs)

{-
rowDeltas takes in a zipped row of (scaleFactor, randomNumber)
pairs and returns a row with integers corresponding to whether
the pixel in that position should be deleted, left alone, or
more inserted next to it.
-}
rowDeltas :: [(Float, Float)] -> [Int]
rowDeltas [] = []
rowDeltas ((sf, rand):restRow)
    | sf <= 1 = maybeDropPixel
    | otherwise = maybeAddPixels
    where
        maybeDropPixel =
            if (1 - sf) / 2 < rand then 0 : (rowDeltas restRow) -- keep pixel
            else -1 : (rowDeltas restRow) -- drop pixel
        maybeAddPixels =
            let certainAdds = floor ((sf - 1) / 2) in
            if (sf - (fromIntegral certainAdds) - 1) < rand then
                certainAdds : (rowDeltas restRow) -- don't add pixel
            else
                (certainAdds + 1) : (rowDeltas restRow) -- add pixel

{-
cmapPixelDeltas converts a full "image" of country codes (cmap)
into one with an integer for deleting / duplicating a pixel.
-}
cmapPixelDeltas scaleFactors cmap =
    map rowDeltas
        (nestedZipper
            (scaleFactorsMap scaleFactors cmap)
            (randFloatsMap 0 cmap))

{-
rescaleRow takes in a list of (countryCode, n :: Integer) tuples,
where n is -1 if a pixel is to be removed, and positive if pixels
should be added.
-}
rescaleRow [] = []
rescaleRow ((ccode, rep) : restRow)
    | rep == -1 = rescaleRow restRow
    | rep == 0 = ccode : (rescaleRow restRow)
    | otherwise = (padBy ccode (rep + 1) []) ++ (rescaleRow restRow)

-- simply map the above over all rows:
rescaleRows seed scaleFactors cmap =
    map rescaleRow
        (nestedZipper cmap (cmapPixelDeltas scaleFactors cmap))

-- THE MAIN DRIVER FUNCTION FOR THE ALGORITHM: --
rescaleAreas seed1 seed2 scaleFactors cmap =
    -- note: do over rows ...
    let withRowsDone = padNestedList "" $ rescaleRows seed1 scaleFactors cmap in
    -- ... then transpose and do over "rows" (now columns) ... 
    let withColumnsDone = padNestedList "" $ rescaleRows seed2 scaleFactors (transposeNestedList withRowsDone) in
    -- ... then transpose the result to get back in original orientation
    transposeNestedList withColumnsDone

{-
processMain :: FilePath -> IO ()
processMain fp = do
    x <- readImage fp
    y <- process x
    pure ()
    where
        process (Left err) = error err
        process (Right im) = do
            print (frequencyMap (colTupleMap convImage))
            pure ()
            where
                convImage = convertRGB8 im
-}