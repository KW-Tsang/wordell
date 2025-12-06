module Main where

import Graphics.Gloss
import Logic
import System.IO


-- scale values
offset:: Int
offset = 100
lettSize:: Float
lettSize = 80


-- create window

window :: Display
window = InWindow "Straight up guessing it rn" (500, 600) (offset, offset)


-- set background colour
background :: Color
background = white

-- text colors
getColour:: LetterPlace -> Color
getColour G = makeColorI 0 128 0 255
getColour Y = makeColorI 128 0 0 255
getColour N = makeColorI 0 0 0 255


-- draw a letter
letter :: Char -> LetterPlace -> Picture
letter a p = pictures [color (getColour p) $ rectangleSolid lettSize lettSize,
    translate (-22) (-25) (scale 0.5 0.5 (color white $ Text [a]))]


pictureGuess:: String -> [LetterPlace] -> Picture
pictureGuess gs ps = pictures (pictureGuess' ((lettSize + 10) * (-2) ) gs ps)

pictureGuess':: Float -> String -> [LetterPlace] -> [Picture]
pictureGuess' _ _ [] = []
pictureGuess' _ "" _ = []
pictureGuess' t (g:gs) (p:ps) = translate t 0 (letter g p) : pictureGuess' (t+10+ lettSize) gs ps


-- draw circle
drawing :: Picture
drawing = pictureGuess "MONAD" [N,G,N,Y,N]


-- get dictionary
type Dictionary = [String]

getDict:: String -> IO Dictionary
getDict path = do
    fileHandle <- openFile path ReadMode
    contents <- hGetContents fileHandle
    return (words contents)


-- main
main :: IO ()
main = do
    --dict <- getDict "assets/test.txt"
    display window background drawing
