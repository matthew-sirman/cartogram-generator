module ProcessMap (
    processMain
) where

import Codec.Picture
import ImageParse
import ImageGen

processMain :: IO ()
processMain = do
    x <- readImage "test.bmp"
    y <- process x
    pure ()
    where 
        process (Left err) = error err
        process (Right im) = do
            print (frequencyMap (colTupleMap convImage))
            pure ()
	        where
	        	convImage = convertRGB8 im