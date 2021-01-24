module Scale where

import DataTools.DataHandle
import Data.Time
import Data.Set (fromList,toList)
import qualified Data.Map as M

{-
getRawData database date = 
    M.fromList [(fst t, (let Just x = (snd t) in x)) | t <- raw, (snd t) /= Nothing]
    where raw = [(countryCode, fetchData database countryCode date) | countryCode <- M.keys (countryData database)]

getTotal rawData =
    (sum . map snd) $ M.assocs rawData
-}

getRelativeStats :: CountryData -> UTCTime -> (M.Map String Float)
getRelativeStats database date =
    M.fromList [(cc,(let Just v = M.lookup cc statistics in v) / totalStats) | cc <- availableCodes]
    
    where raw = [(countryCode, fetchData database countryCode date) | countryCode <- M.keys (countryData database)]
          statistics = M.fromList [(cc, (let Just x = v in x)) | (cc, v) <- raw, v /= Nothing]
          availableCodes = map fst (M.assocs statistics)
          totalStats = (sum . map snd) $ M.assocs statistics

getStatisticScales countryAreas database date =
    M.fromList
        (map
            (\(ccode, area) ->
                (ccode, --if ccode == "ISR" then 10 else 1))
                    totalArea * (M.findWithDefault 0 ccode relStats) / area))
            countryAreaList)
    where
        countryAreaList = M.assocs countryAreas
        totalArea = sum (map snd countryAreaList)
        relStats = getRelativeStats database date
