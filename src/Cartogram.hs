module Cartogram where

import System.IO
import Csv.Parser
import Data.Time
import qualified Data.Map as M
import Scale
import ImageParse
import ImageGen
import Codec.Picture
import DataTools.DataHandle
import ProcessMap

-- Make Cartogram - takes a colour map file path, a database file path,
--  format, and a date
makeCartogram :: FilePath -> FilePath -> FilePath -> String -> UTCTime -> FilePath -> IO ()
makeCartogram cMapFile mapFile dbFile fmt date destFile = do
    -- load database
    database <- loadData dbFile fmt
    -- scale <- pure $ getStatisticScales database date
    
    -- rawData <- pure $ getRawData database date
    -- print rawData
    -- print (getTotal rawData)

    -- load map picture
    mapImage <- readImage mapFile

    -- load colour to country map
    rawCsv <- readFile cMapFile
    case runParser (csvParser ';' False) rawCsv of
        Left (Just err) -> error err
        Left Nothing -> error "Unknown parser error"
        Right (_, cMapCsv) -> do
            processed <- process mapImage $ M.fromList [(let pVal = read (row !! 2) :: Pixel8 in (pVal, pVal, pVal), row !! 0) | row <- records cMapCsv]
            
            scale <- pure $ getStatisticScales (frequencyMap processed) database date

            --print scale

            writePng destFile $ 
                getImageFromList $  
                countryMapToColorMap $
                rescaleAreas 0 0 scale processed
                
            pure ()
            
    where
        process (Left err) _ = error err
        process (Right im) colToCode = pure $ countryMap (convertRGB8 im) colToCode

{-
        remapScale csv scale = M.fromList [(codeToPixel code, val) | (code, val) <- M.assocs scale]
            where
                codeToPixel :: String -> (Pixel8, Pixel8, Pixel8)
                codeToPixel c = case lookup c (map (\r -> (r !! 0, r !! 2)) $ records csv) of
                    Nothing -> (255, 0, 0) -- error $ "No key found for '" ++ c ++ "'!"
                    Just c' -> let pVal = read c' :: Pixel8 in (pVal, pVal, pVal)
-}
