import System.IO
import Control.Monad
import Data.List

main :: IO ()
main = do
  contents <- readFile "words_alpha.txt"
  putStrLn (intercalate "\n" (i18n . lines contents))

i18n :: [String] -> [String]
i18n = stab . shrt . prune . concat . comb 

-- strings and their abbreviations
stab :: [String] -> [String]
stab xs = zipWith (\a b -> a ++ " " ++ b) xs (map abbr xs)

-- abbreviation
abbr :: String -> String
abbr x = [head x] ++ (show (length x - 2)) ++ [last x]

-- shortest words, alphabetically
shrt :: [String] -> [String]
shrt xs = sortBy (\a b -> compare (length a) (length b)) (sort xs)

-- non trivial words
prune :: [String] -> [String]
prune xs = filter (\x -> length x > 3) xs

-- first-last letter combo, find unique middle letter counts
comb :: [String] -> [[String]]
comb xs = [uniq $ fl a b xs | a <- ['a'..'z'], b <- ['a'..'z']]

-- words with first-last letter combo
fl :: Char -> Char -> [String] -> [String]
fl a b xs = [x | x <- xs, a == head x, b == last x]

-- words with unique middle letter counts
uniq :: [String] -> [String]
uniq xs = filter (\x -> (midc x xs) == 1) xs

-- how many times the middle count appears
midc :: String -> [String] -> Int
midc x xs = (length . filter (\y -> mid x == mid y)) xs

-- middle count
mid :: String -> Int
mid x = (length x) - 2
