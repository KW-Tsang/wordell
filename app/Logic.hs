module Logic where

-- datatype for letter guesses
data LetterPlace = N | Y | G

lpToChar:: LetterPlace -> Char
lpToChar N = 'N'
lpToChar Y = 'Y'
lpToChar G = 'G'

lpToString:: [LetterPlace] -> String
lpToString = foldr ((:) . lpToChar) ""

-- remove an element from list
removeFrom:: Eq a => a -> [a] -> [a]
removeFrom _ [] = []
removeFrom x (a:as)
    | x == a = as
    | otherwise = a : removeFrom x as

-- full checker
checkGuess':: String -> String -> String -> [LetterPlace]
checkGuess' "" _ _ = []
checkGuess' _ _ "" = []
checkGuess' (a:as) ans (g:gs)
    | a == g = G : nxt
    | g `elem` ans = Y : nxt
    | otherwise = N : checkGuess' as ans gs
    where
        nxt = checkGuess' as (removeFrom g ans) gs


checkGuess:: String -> String -> [LetterPlace]
checkGuess ans = checkGuess' ans ans
