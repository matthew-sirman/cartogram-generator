-- module Helper.MatchGenerator (
--     matchMain
-- ) where

module Helper.MatchGenerator where

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
    (pictures $ map (createEntry totalHeight) (zip [1..] (records csv)), totalHeight)

    where
        createEntry h (index, row) =
            pictures $ map (translate 0 (fromIntegral index * (-cellHeight) + fromIntegral h / 2))
                [ nameText (row !! 0)
                , colourSquare (genColour index)
                ]
        nameText = translate (-150) 0 . scale 0.1 0.1 . text
        colourSquare col = translate 100 0 $ color col $ rectangleSolid 25 25
        genColour i = makeColorI i i i 255

        totalHeight = (length (records csv) + 2) * cellHeight

        cellHeight :: Num a => a
        cellHeight = 30

createMapFile :: CSV -> String
createMapFile csv = concat $ map (writeLine . mkRecord) (zip [1..] (records csv))
    where
        mkRecord (index, rec) = [rec !! 2, rec !! 0, show index]

        writeLine [] = "\n"
        writeLine (s:ss) 
            | ss == [] = s ++ writeLine ss
            | otherwise = s ++ ";" ++ writeLine ss

resFolder :: FilePath
resFolder = "res/"

matchMain :: IO ()
matchMain = do
    rawFile <- readFile countriesFile
    case runParser (csvParser ';' True) (rawFile ++ "\n") of
        Left (Just err) -> error err
        Left Nothing -> error "Unknown error!"
        Right (_, csv) -> do 
            let (p, h) = createHelperImage csv in 
                exportPictureToFormat
                    writePng
                    (400, h)
                    (light blue)
                    imageOutput
                    p
            writeFile mappingOutput $ createMapFile csv

    where
        countriesFile = resFolder ++ "countries.csv"
        imageOutput = resFolder ++ "imageHelper.png"
        mappingOutput = resFolder ++ "mapping.csv"

