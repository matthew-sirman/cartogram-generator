module ProcessMap where

import Codec.Picture
import ImageParse
import ImageGen
import System.Random
import qualified Data.Map as M
import Debug.Trace

transposeNestedList nl =
    if null (head nl) then [] else
        (map head nl) : (transposeNestedList (map tail nl))

padBy paddingEl 0 l = l
padBy paddingEl n l
    | n < 0 = l
    | otherwise = paddingEl : (padBy paddingEl (n - 1) l)

nlPadder paddingEl [] [] = []
nlPadder paddingEl (toAdd:addNums) (l:ls) =
    (padBy paddingEl toAdd l) : (nlPadder paddingEl addNums ls)

{-
padNestedList takes a list of lists nl and adds paddingEl
to the front of each list until the length of each list in the list
is the maximum length of any list in the list.
-}
padNestedList paddingEl nl =
    -- map reverse (nlPadder paddingEl deltas (map reverse nl))
    map (\(r, d) -> leftPadding d ++ r ++ rightPadding d) $ zip nl deltas

    where
        lengths = map length nl
        maxlen = maximum lengths
        deltas = map (\len -> maxlen - len) lengths

        leftPadding d = take (floor $ fromIntegral d / 2) $ repeat paddingEl
        rightPadding d = take (ceiling $ fromIntegral d / 2) $ repeat paddingEl


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

randList :: [Float] -> Int -> Int -> [Int]
randList rands 0 1  = [0]
randList rands 1 1 = [1]
randList rands totalOnes listLength =
    let r = head rands in
    let p = (fromIntegral totalOnes) / (fromIntegral listLength) in
    if r < p then
        1 : (randList (tail rands) (totalOnes - 1) (listLength - 1))
    else
        0 : (randList (tail rands) totalOnes (listLength - 1))

randListWithSum :: [Float] -> Int -> Int -> [Int]
randListWithSum rands sum listLength =
    let asum = abs sum
        certain = asum `div` listLength
        totalProbAdds = asum - certain * listLength
        lst = zipWith (+) (take listLength $ repeat certain) (randList rands totalProbAdds listLength) in
    if sum >= 0 then lst else (map (\x -> -1 * x) lst)

{-
rowDeltas takes in a zipped row of (scaleFactor, randomNumber)
pairs and returns a row with integers corresponding to whether
the pixel in that position should be deleted, left alone, or
more inserted next to it.
-}
rowDeltas :: [(Float, Float)] -> [Int]
rowDeltas [] = []
rowDeltas ((sf, rand):restRow) =
    (nPixels - 1) : (rowDeltas restRow)
    where
        nPixels = floor $ logBase (1 - p) (1 - rand)
        p = 1 / (1 + sf)

   --  | sf <= 1 = maybeDropPixel
   --  | otherwise = maybeAddPixels
   --  where
   --      maybeDropPixel =
   --          if (1 - sf) / 2 < rand then 0 : (rowDeltas restRow) -- keep pixel
   --          else -1 : (rowDeltas restRow) -- drop pixel
   --      maybeAddPixels =
   --          let certainAdds = floor ((sf - 1) / 2) in
   --          if (sf - (fromIntegral certainAdds) - 1) < rand then
   --              certainAdds : (rowDeltas restRow) -- don't add pixel
   --          else
   --              (certainAdds + 1) : (rowDeltas restRow) -- add pixel


rowDeltas2 :: M.Map String [Int] -> [String] -> [Int] -> (M.Map String [Int], [Int])
rowDeltas2 countryPixelLists [] acc =
    (countryPixelLists, reverse acc)
rowDeltas2 countryPixelLists (ccode:ccodes) acc =
    let pDelta : restPList = M.findWithDefault [0] ccode countryPixelLists in
    rowDeltas2
        (M.insert ccode restPList countryPixelLists)
        ccodes
        (pDelta : acc)

cmapPixelDeltas2 countryPixelLists [] acc = reverse acc 
cmapPixelDeltas2 countryPixelLists (row:rows) acc =
    let (newPixelLists, calcDeltas) = rowDeltas2 countryPixelLists row [] in
    cmapPixelDeltas2 newPixelLists rows (calcDeltas : acc)

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

rescaleRows2 countryPixelLists cmap =
    let x = (nestedZipper cmap (cmapPixelDeltas2 countryPixelLists cmap [])) in
    map rescaleRow x

-- THE MAIN DRIVER FUNCTION FOR THE ALGORITHM: (OLD) --
rescaleAreas seed1 seed2 scaleFactors cmap =
    -- note: do over rows ...
    let withRowsDone = padNestedList "" $ rescaleRows seed1 scaleFactors cmap in
    -- ... then transpose and do over "rows" (now columns) ... 
    let withColumnsDone = padNestedList "" $ rescaleRows seed2 scaleFactors (transposeNestedList withRowsDone) in
    -- ... then transpose the result to get back in original orientation
    transposeNestedList withColumnsDone

getCountryPixelLists rands countryAreas pixelDeltasMap =
    M.fromList
        (map
            (\(ccode, area) ->
                let toAdd = M.findWithDefault 0 ccode pixelDeltasMap in
                (ccode, (randListWithSum rands toAdd area)))
            (M.assocs countryAreas))

rescaleAreas2Round seed1 seed2 pixelDeltasMap cmap =
    let countryPixelLists1 = getCountryPixelLists (randoms (mkStdGen seed1)) (frequencyMap cmap) pixelDeltasMap
        withRowsDone = padNestedList "" $ rescaleRows2 countryPixelLists1 cmap
        countryPixelLists2 = getCountryPixelLists (randoms (mkStdGen seed2)) (frequencyMap withRowsDone) pixelDeltasMap
        withColumnsDone = padNestedList "" $ rescaleRows2 countryPixelLists2 (transposeNestedList withRowsDone) in
    transposeNestedList withColumnsDone

rescaleAreas2Rounds intRands pixelDeltasMap cmap rounds =
    if rounds == 0 then
        cmap
    else
        let r1:r2:restRands = intRands in
        rescaleAreas2Rounds
            restRands
            pixelDeltasMap
            (rescaleAreas2Round r1 r2 pixelDeltasMap cmap)
            (rounds - 1)

countryPixelDeltas :: M.Map String Float -> M.Map String Int -> M.Map String Int
countryPixelDeltas scaleFactors countryAreas =
    M.fromList
        (map
            (\(ccode, area) ->
                let sf = (M.findWithDefault 0 ccode scaleFactors) in
                (ccode, floor ((sf - 1) * fromIntegral area)))
            (M.assocs countryAreas))

rescaleAreas2 :: Int -> M.Map String Float -> [[String]] -> Int -> [[String]]
rescaleAreas2 seed scaleFactors cmap rounds =
    rescaleAreas2Rounds
        (randoms (mkStdGen seed))
        (countryPixelDeltas scaleFactors (frequencyMap cmap))
        cmap
        rounds

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
