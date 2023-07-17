import System.Environment
import Data.List

main :: IO ()
main = do
  args <- getArgs
  file <- readFile $ head args
  disp . d29e . lines $ file

disp xs = putStr $ intercalate "\n" xs

d29e :: [String] -> [String]
d29e = stab . comb

-- strings and their abbreviations
stab :: [String] -> [String]
stab xs = zipWith (\a b -> a ++ " " ++ b) xs (map abbr xs)

-- abbreviation
abbr :: String -> String
abbr x = [head x] ++ (show (length x - 2)) ++ [last x]

-- sorted, grouped, unique words
comb :: [String] -> [String]
comb xs = map (\(_,_,_,x) -> x) $ concat $ uniq $ sort $ map lab $ xs

-- unique words
uniq :: [(Int, Char, Char, String)] -> [[(Int, Char, Char, String)]]
uniq xs = filter (\xs -> length xs == 1) $ groupBy (\(a,b,c,_) (d,e,f,_) -> (a,b,c) == (d,e,f)) $ xs

-- label string with leng first last
lab :: String -> (Int, Char, Char, String)
lab x = (length x, head x, last x, x)
