module Helper.MatchGenerator (
    matchMain
) where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Export.Image
import Codec.Picture
import Csv.Parser

-- loadTextFileStrings :: FilePath -> IO [String]
-- loadTextFileStrings fp = do
--     raw <- readFile fp
--     pure $ lines raw

-- Takes country list
createHelperImage :: CSV -> (Picture, Int)
createHelperImage csv = 
    (pictures $ map (createEntry totalHeight) (zip [0..] (records csv)), totalHeight cs)

    where
        createEntry h (index, row) =
            pictures $ map (translate 0 (fromIntegral (index + 1) * (-cellHeight) + fromIntegral h / 2))
                [ nameText (row !! 0)
                , colourSquare (genColour index)
                ]
        nameText = translate (-150) 0 . scale 0.1 0.1 . text
        colourSquare col = translate 100 0 $ color col $ rectangleSolid 25 25
        genColour i = makeColorI i i i 255

        totalHeight cs = (length cs + 2) * cellHeight

        cellHeight :: Num a => a
        cellHeight = 30

resFolder :: FilePath
resFolder = "res/"

countriesFile :: FilePath
countriesFile = resFolder ++ "countries.csv"

imageOutput :: FilePath
imageOutput = resFolder ++ "imageHelper.png"

mappingOutput :: FilePath
mappingOutput = resFolder ++ "mapping.txt"

matchMain :: IO ()
matchMain = do
    rawFile <- readFile countriesFile
    (p, h) <- createHelperImage countriesFile
    exportPictureToFormat
        writePng
        (400, h)
        (light blue)
        imageOutput
        p
    
