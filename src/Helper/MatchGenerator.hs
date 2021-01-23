module Helper.MatchGenerator (

) where

import qualified Graphics.Gloss as G
import Graphics.Gloss.Juicy

loadTextFileStrings :: FilePath -> IO [String]
loadTextFileStrings fp = do
    raw <- readFile fp
    pure $ lines raw

-- Takes country list
createHelperImage :: [String] -> Picture
createHelperImage cs = G.pictures $ map createEntry $ zip [0..] cs
    where
        createEntry (index, country) = G.pictures 
            [ nameText country
            , colourSquare $ genColour index
            ]


