main = do
  a <- read <$> getLine :: IO Int
  putStrLn $ outputMessage a

outputMessage :: Int -> String
outputMessage n
  | even n && n > 2 = "YES"
  | otherwise = "NO"
