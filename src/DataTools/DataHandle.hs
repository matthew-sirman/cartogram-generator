module DataTools.DataHandle(
      CountryData(..)
    , loadData
    , fetchData
) where

import Csv.Parser
import qualified Data.Map as M
import Data.Time
import Data.List (sort)
import System.IO
import System.Directory (listDirectory)

data CountryData = CountryData { countryData :: M.Map String (M.Map UTCTime Float) } deriving Show

countryToCode :: IO (M.Map String String)
countryToCode = do
    contents <- readFile "res/vaccinations/locations.csv"
    
    pure $ process contents
    where
        process :: String -> M.Map String String
        process fd = case runParser (csvParser ',' True) fd of
                        Left (Just err) -> error err
                        Left Nothing -> error "Unkown parser error"
                        Right (_, csv) -> M.fromList $ zip [record !! 0 | record <- records csv] [record !! 1 | record <- records csv]
    
    

-- Parse date with given date format
parseDate :: String -> String -> UTCTime
parseDate dateString dateFormat = parseTimeOrError True defaultTimeLocale dateFormat dateString

-- Parses a specific file
parseFile :: FilePath -> String -> IO ((String, (M.Map UTCTime Float)))
parseFile filename dateFormat = do
    contents <- readFile filename
    pure $ process contents
    
    where
        process :: String -> (String, (M.Map UTCTime Float))
        process fd = case runParser (csvParser ',' True) fd of
                        Left (Just err) -> error err
                        Left Nothing -> error "Unkown parser error"
                        Right (_, csv) -> (country, M.fromList (zip [getDate row | row <- records csv] [getStatistic row | row <- records csv]))
                                            where country = ((records csv) !! 0) !! 0
                                                  getDate row = parseDate (row !! 1) dateFormat
                                                  getStatistic row = read (row !! ((length row) - 3)) :: Float
    
    
-- Loads data from directory with csv names as {Country}.csv
loadData :: FilePath -> String -> IO (CountryData)
loadData directory dateFormat = do
    fileNames <- listDirectory directory
    dataContent <- sequence [parseFile (directory ++ fileName) dateFormat | fileName <- fileNames]
    
    processedData <- process dataContent
    
    pure $ CountryData { countryData = processedData }
    
    where
        process :: [(String, (M.Map UTCTime Float))] -> IO (M.Map String (M.Map UTCTime Float))
        process dataContent = do
            countryCodes <- countryToCode
            pure $ M.fromList [(let Just countryCode = M.lookup (fst dataPoint) countryCodes in (countryCode, snd dataPoint)) | dataPoint <- dataContent, M.member (fst dataPoint) countryCodes]

-- Interpolates the statistic between two know data points
interpolate :: (UTCTime, Float) -> (UTCTime, Float) -> UTCTime -> Float
interpolate (d1, f1) (d2, f2) d = f1 + (f2 - f1) * deltaT / totalT
    where deltaT = fromIntegral $ round $ nominalDiffTimeToSeconds (diffUTCTime d d1)
          totalT = fromIntegral $ round $ nominalDiffTimeToSeconds (diffUTCTime d2 d1)

-- Fetches data given a CountryData database, country code, and UTCTime
fetchData :: CountryData -> String -> UTCTime -> Maybe Float
fetchData dataContent countryCode date = do
    content <- M.lookup countryCode (countryData dataContent)
    let sortedDates = sort (M.assocs content)
        p1 = last (filter (<= (date, 0)) sortedDates)
        p2 = head (filter (> (date, 0)) sortedDates) in if (null sortedDates || fst (head sortedDates) > date || fst (last sortedDates) < date) then Nothing else Just (interpolate p1 p2 date)