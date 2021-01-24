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

-- Merges two maps giving priority to first
forgetfulMerge :: (Ord a, Ord c) => M.Map a (M.Map c d) -> M.Map a (M.Map c d) -> M.Map a (M.Map c d)
forgetfulMerge m1 m2 = M.unionWith (\x -> \y -> M.unionWith (\a -> \b -> a) x y) m1 m2

-- Parses a specific file
-- dataFormat is of the form [country_index, date_index, statistic_index]
parseFile :: FilePath -> String -> [Int] -> IO (M.Map String (M.Map UTCTime Float))
parseFile filename dateFormat dataFormat = do
    contents <- readFile filename
    pure $ process contents
    
    where
        process :: String -> M.Map String (M.Map UTCTime Float)
        process fd = case runParser (csvParser ',' True) fd of
                        Left (Just err) -> error err
                        Left Nothing -> error "Unkown parser error"
                        Right (_, csv) -> foldl forgetfulMerge M.empty [M.fromList [(getCountry row, M.fromList [(getDate row, getStatistic row)])] | row <- records csv]
                                            where getCountry row = row !! ((dataFormat !! 0) `mod` (length row))
                                                  getDate row = parseDate (row !! ((dataFormat !! 1) `mod` (length row))) dateFormat
                                                  getStatistic row = read (row !! ((dataFormat !! 2) `mod` (length row))) :: Float
    
    
-- Loads data from directory with csv names as {Country}.csv
loadData :: FilePath -> String -> [Int] -> IO (CountryData)
loadData directory dateFormat dataFormat = do
    fileNames <- listDirectory directory
    dataContent <- sequence [parseFile (directory ++ fileName) dateFormat dataFormat | fileName <- fileNames]
    
    processedData <- process dataContent
    
    pure $ CountryData { countryData = processedData }
    
    where
        process :: [M.Map String (M.Map UTCTime Float)] -> IO (M.Map String (M.Map UTCTime Float))
        process dataContent = do
            countryCodes <- countryToCode
            pure $ M.fromList [(let Just countryCode = M.lookup (fst dataPoint) countryCodes in (countryCode, snd dataPoint)) | dataPoint <- mergedDataContent, M.member (fst dataPoint) countryCodes]
            where
                mergedDataContent = M.assocs $ foldl forgetfulMerge M.empty dataContent

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