module Scale where

import DataTools.DataHandle
import Data.Time
import Data.Maybe (catMaybes)
import Data.Set (fromList,toList)
import qualified Data.Map as M

getStatisticScales :: CountryData -> UTCTime -> (M.Map String Float)
getStatisticScales database date =
    M.fromList [(cc, (let Just v = M.lookup cc statistics in v) / totalStats) | cc <- availableCodes]
    
    where appearingCodes = (toList . fromList) $ map fst (M.assocs (countryData database))
          raw = [(countryCode, fetchData database countryCode date) | countryCode <- appearingCodes]
          statistics = M.fromList [(cc, (let Just x = v in x)) | (cc, v) <- raw, v /= Nothing]
          availableCodes = map fst (M.assocs statistics)
          totalStats = (sum . map snd) $ M.assocs statistics