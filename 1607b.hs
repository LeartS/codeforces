import Data.Foldable (for_)
next :: Int -> Int -> Int
next position jumpSize
  | even position = position - jumpSize
  | otherwise = position + jumpSize

solve :: Int -> Int -> Int
solve start jumps
  | m == 0 = start
  | otherwise = foldl next start [(jumps - m + 1)..jumps]
  where m = mod jumps 4

testCase :: IO ()
testCase = do
  line <- getLine;
  let [start, jumps] = map read (words line);
  print $ solve start jumps;

main = do
  line <- getLine
  let nTestCases = read line :: Integer
  for_ [1..nTestCases] (const testCase)
