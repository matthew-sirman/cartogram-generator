module Helper.MatchGenerator (
    matchMain
) where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Export.Image
import Codec.Picture

loadTextFileStrings :: FilePath -> IO [String]
loadTextFileStrings fp = do
    raw <- readFile fp
    pure $ lines raw

-- Takes country list
createHelperImage :: FilePath -> IO (Picture, Int)
createHelperImage fp = do
    cs <- loadTextFileStrings fp
    pure (pictures $ map (createEntry (totalHeight cs)) (zip [0..] cs), totalHeight cs)

    where
        createEntry h (index, country) =
            pictures $ map (translate 0 (fromIntegral (index + 1) * (-cellHeight) + fromIntegral h / 2))
                [ nameText country
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
countriesFile = resFolder ++ "countries.txt"

imageOutput :: FilePath
imageOutput = resFolder ++ "imageHelper.png"

mappingOutput :: FilePath
mappingOutput = resFolder ++ "mapping.txt"

matchMain :: IO ()
matchMain = do
    (p, h) <- createHelperImage countriesFile
    exportPictureToFormat
        writePng
        (400, h)
        (light blue)
        imageOutput
        p
    
