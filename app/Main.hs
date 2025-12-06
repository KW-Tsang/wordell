module Main where

import Graphics.Gloss

-- create window
window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)

-- set background colour
background :: Color
background = white

-- draw circle
drawing :: Picture
drawing = circle 80

-- main
main :: IO ()
main = display window background drawing
