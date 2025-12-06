module Logic where

-- datatype for letter guesses
data LetterPlace = N | Y | G

lpToChar:: LetterPlace -> Char
lpToChar N = 'N'
lpToChar Y = 'Y'
lpToChar G = 'G'

lpToString:: [LetterPlace] -> String
lpToString [] = ""
lpToString (l:ls) = (lpToChar l):(lpToString ls)

-- see if words' letters match
checkGreen:: String -> String -> [LetterPlace]
checkGreen _ "" = []
checkGreen "" _ = []
checkGreen (a:as) (g:gs)
    | a == g = G : (checkGreen as gs)
    | otherwise = N : (checkGreen as gs)


-- remove an element from list
removeFrom:: Eq a => a -> [a] -> [a]
removeFrom x [] = []
removeFrom x (a:as)
    | x == a = as
    | otherwise = a : (removeFrom x as)


-- see if letters are in words
checkYellow:: String -> String -> [LetterPlace]
checkYellow _ "" = []
checkYellow ans (g:gs)
    | elem g ans = Y : (checkYellow (removeFrom g ans) gs)
    | otherwise = N : (checkYellow ans gs)
