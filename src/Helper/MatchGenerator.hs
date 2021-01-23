module Helper.MatchGenerator (
    createHelperImage
) where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

loadTextFileStrings :: FilePath -> IO [String]
loadTextFileStrings fp = do
    raw <- readFile fp
    pure $ lines raw

-- Takes country list
createHelperImage :: FilePath -> IO Picture
createHelperImage fp = do
    cs <- loadTextFileStrings fp
    pure $ pictures $ map createEntry $ zip [0..] cs

    where
        createEntry (index, country) = pictures $ map (translate 0 (fromIntegral index * (-30)))
            [ nameText country
            , colourSquare (genColour index)
            ]
        nameText = translate (-100) 0 . scale 0.1 0.1 . text
        colourSquare col = translate 200 0 $ color col $ rectangleSolid 25 25
        genColour i = makeColorI i i i 255
