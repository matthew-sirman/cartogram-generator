module DataHandle(
	  CountryData(..)
	, loadData
	, fetchData
) where

import Csv.Parser
import qualified Data.Map as M
import Data.Time
import System.IO

data CountryData = CountryData { countryData :: M String (M UTCTime Float) }

countryToCode :: IO (M String String)
countryToCode = do
	contents <- readFile "../res/vaccinations/locations.csv"
	
	pure $ process contents
	where
		process :: String -> M String String
		process fd = case runParser (csvParser ',' True) fd of
						Left err -> error err
						Right (_, csv) -> M.fromList $ zip [record !! 0 | record <- records csv] [record !! 1 | record <- records csv]
	
	

-- Parse date with given date format
parseDate :: String -> String -> UTCTime
parseDate dateString dateFormat = parseTimeOrError True defaultTimeLocale dateFormat dateString

-- Parses a specific file
parseFile :: String -> IO ((String, (M UTCTime Float)))
parseFile filename dateFormat = do
	contents <- readFile filename
	pure $ process contents
	
	where
		process :: String -> (String, (M UTCTime Float))
		process fd = case runParser (csvParser ',' True) fd of
						Left err -> error err
						Right (_, csv) -> (country, M.fromList zip [getDate row | row <- records csv] [getStatistic row | row <- records csv])
											where country = ((records csv) !! 0) !! 0
												  getDate row = parseDate (row !! 1) dateFormat
												  getStatistic row = read (row !! 4) :: Float
	
	
-- Loads data from directory with csv names as {Country}.csv
loadData :: String -> IO (CountryData)
loadData directory dateFormat = do
	fileNames <- listDirectory directory
	dataContent <- sequence [parseFile fileName dateFormat | fileName <- fileNames]
	
	processedData <- process dataContent
	
	pure $ CountryData { countryData = processedData }
	
	where
		process :: [(String, (M UTCTime Float))] -> IO (M String (M UTCTime Float))
		process dataContent = do
			cntryCode <- countryToCode
			pure $ M.fromList [(cntryCode !! (fst dataPoint), snd dataPoint) | dataPoint <- dataContent]

-- Interpolates the statistic between two know data points
interpolate :: (UTCTime, Float) -> (UTCTime, Float) -> UTCTime -> Float 
interpolate (d1, f1) (d2, f2) d = f1 + (f2 - f1) * deltaT / totalT
	where deltaT = nominalDiffTimeToSeconds (diffUTCTime d d1)
		  totalT = nominalDiffTimeToSeconds (diffUTCTime d2 d1)

-- Fetches data given a CountryData database, country code, and UTCTime
fetchData :: CountryData -> String -> UTCTime -> Maybe Float
fetchData dataContent countryCode date = do
	database <- countryData dataContent
	case M.lookup countryCode database of
		Just content -> | null sortedDates -> Nothing
						| head sortedDates > date -> Nothing
						| last sortedDates < date -> Nothing
						| otherwise -> Just (interpolate (d1, database !! d1) (d2, database !! d2) date)
							where d1 = last (filter (<= date) sortedDates)
								  d2  = head (filter (> date) sortedDates)
					where sortedDates <- sort [fst dataPoint | dataPoint <- M.assocs content]
		Nothing -> Nothing