module Main where

import Codec.Picture
import ImageParse
import ImageGen
import ProcessMap
import Helper.MatchGenerator
import Cartogram
import Data.Time
import DataTools.DataHandle

resDir :: FilePath
resDir = "res/"

main :: IO ()
main = do
    database <- loadData dbFile fmt dataFormat
    
    makeCartogram cMapFile mapFile database date destFile
    where
        cMapFile = resDir ++ "mapping.csv"
        mapFile = resDir ++ "worldmap225.png"
        -- COVID data
        -- dbFile = resDir ++ "vaccinations/country_data/"
        -- fmt = "%Y-%m-%d"
        -- dataFormat = [0, 1, -3]
        -- CO2 data
        dbFile = resDir ++ "CO2/"
        fmt = "%Y"
        dataFormat = [0, 1, 2]
        date = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" "2021-01-15"
        destFile = "res/testCarto.png"