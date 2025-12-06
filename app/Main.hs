module Main where

import Graphics.Gloss
import Logic
import System.IO
import Graphics.Gloss.Interface.IO.Interact
import System.Exit (exitSuccess)
import GHC.Float (int2Float)
import Data.Char
--import System.Random


-- scale values
offset:: Int
offset = 100
lettSize:: Float
lettSize = 80


-- create window

window:: Display
window = InWindow "Straight up guessing it rn" (500, 800) (offset, offset)


-- set background colour
background:: Color
background = white

-- text colors
getColour:: LetterPlace -> Color
getColour G = makeColorI 0 128 0 255
getColour Y = makeColorI 128 0 0 255
getColour N = makeColorI 0 0 0 255



-- draw the guess slots
boxes:: Float -> [Picture]
boxes 0 = []
boxes n = pictures (boxRow ((lettSize + 10)*(-2) + (n-1)*(lettSize +10)) 5) : boxes (n-1)

boxRow:: Float -> Float -> [Picture]
boxRow _ 0 = []
boxRow h n = translate ((lettSize + 10)*(n-3)) h
    (circle (lettSize*0.5)) : boxRow h (n-1)



-- draw a previous guesses
pictureGuesses:: [(String, [LetterPlace])] -> [Picture]
pictureGuesses [] = []
pictureGuesses ((g,ps):gs) = pictureGuesses' g ps : pictureGuesses gs

pictureGuesses':: String -> [LetterPlace] -> Picture
pictureGuesses' gs ps = pictures (pictureGuesses'' ((lettSize + 10) * (-2)) gs ps)

pictureGuesses'':: Float -> String -> [LetterPlace] -> [Picture]
pictureGuesses'' _ _ [] = []
pictureGuesses'' _ "" _ = []
pictureGuesses'' t (g:gs) (p:ps) = translate t 0 (letter g p) :
    pictureGuesses'' (t+10+ lettSize) gs ps

letter:: Char -> LetterPlace -> Picture
letter a p = pictures [color (getColour p) $ circleSolid lettSize,
    translate (-22) (-25) ( scale 0.5 0.5 (color white $ Text [a]) ) ]



-- draw current guess
pictureCurr:: Float -> Float -> String -> [Picture]
pictureCurr _ _ "" = []
pictureCurr h n (c:cs) = translate ((lettSize+10)*(3-n) -22) ((lettSize+10)*(h+3) -25) (scale 0.5 0.5 (Text [c])) :
    pictureCurr h (n-1) cs




-- game states
-- dict, answer, guesses, current input, game is done
data GameState = Init |
    Current Dictionary String [(String, [LetterPlace])] String Bool |
    Over String

-- handle keys
onEvent:: Event -> GameState -> GameState
onEvent (EventKey (Char c) Down _ _) (Current dict ans prevs curr b)
    | ord c > 96 && ord c < 123 && length curr < 5 = Current dict ans prevs (toUpper c:curr) b
    | otherwise = Current dict ans prevs curr b
onEvent (EventKey (SpecialKey KeyDelete) Down _ _) (Current dict ans prevs (_:curr) b)
    | not (null curr) = Current dict ans prevs curr b
    | otherwise = Current dict ans prevs curr b
onEvent (EventKey (SpecialKey KeyEnter) Down _ _) (Current dict ans prevs curr b)
    | length curr == 5 && curr `elem` dict =
        Current dict ans ((curr, checkGuess ans curr):prevs) "" (all isCorrect (checkGuess ans curr))
    | otherwise = Current dict ans prevs curr b
onEvent (EventKey (SpecialKey KeyEsc) _ _ _) s = quitApp exitSuccess s
onEvent _ s = s


quitApp:: IO a -> GameState -> GameState
quitApp _ s = s


-- update
update:: Float -> GameState -> GameState
update _ s = s



-- game over screen
gameOver:: Picture
gameOver = Text "MONAD"

-- render a gamestate
render:: GameState -> Picture
render (Current _ _ prevs curr _) =
    pictures [pictures (boxes 6),
        pictures (pictureGuesses prevs),
        pictures (pictureCurr (int2Float (length prevs)) 5 (reverse curr))]
render _ = gameOver





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
    dict <- getDict "assets/test.txt"
    play window background 30
        (Current dict
            (dict !! 4)
            []
            ""
            False)
        render onEvent update
