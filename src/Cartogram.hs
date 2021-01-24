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
makeCartogram :: FilePath -> FilePath -> FilePath -> String -> UTCTime -> IO ()
makeCartogram cMapFile mapFile dbFile fmt date = do
    -- load database
    database <- loadData dbFile fmt
    scale <- pure $ getStatisticScales database date

    -- load map picture
    mapImage <- readImage mapFile
    processed <- process mapImage

    -- load colour to country map
    rawCsv <- readFile cMapFile
    case runParser (csvParser ';' False) rawCsv of
        Left (Just err) -> error err
        Left Nothing -> error "Unknown error"
        Right (_, cMapCsv) -> do
            pure ()
            writePng "res/testCarto.png" $ 
                getImageFromList $ 
                map (\row -> map (\(r, g, b) -> PixelRGB8 r g b) row) $ 
                processed
                -- rescaleAreas 0 0 (remapScale cMapCsv scale) processed
            
    where
        process (Left err) = error err
        process (Right im) = do
            pure $ colTupleMap $ convertRGB8 im

        remapScale csv scale = M.fromList [(codeToPixel code, val) | (code, val) <- M.assocs scale]
            where
                codeToPixel :: String -> (Pixel8, Pixel8, Pixel8)
                codeToPixel c = case lookup c (map (\r -> (r !! 0, r !! 2)) $ records csv) of
                    Nothing -> (255, 0, 0) -- error $ "No key found for '" ++ c ++ "'!"
                    Just c' -> let pVal = read c' :: Pixel8 in (pVal, pVal, pVal)
    
