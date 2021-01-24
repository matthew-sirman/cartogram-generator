module Main where

import Codec.Picture
import ImageParse
import ImageGen
import ProcessMap
import Helper.MatchGenerator
import Cartogram
import Data.Time

resDir :: FilePath
resDir = "res/"

main :: IO ()
main = makeCartogram cMapFile mapFile dbFile fmt date
    where
        cMapFile = resDir ++ "mapping.csv"
        mapFile = resDir ++ "worldmap225.png"
        dbFile = resDir ++ "vaccinations/country_data/"
        fmt = "%Y-%m-%d"
        date = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" "2021-01-15"
