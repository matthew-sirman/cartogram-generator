module Main where

import Graphics.Gloss
import Helper.MatchGenerator

window :: Display
window = InWindow "Helper" (200, 200) (200, 200)

background :: Color
background = blue

main :: IO ()
main = do
    p <- createHelperImage "countries.txt"
    display window background p
    
