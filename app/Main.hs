module Main where

-- Function to check if a number is prime
isPrime :: Int -> Bool
isPrime n
  | n <= 1 = False  -- 1 and less are not prime
  | otherwise = all (\d -> n `mod` d /= 0) [2..floor (sqrt (fromIntegral n))]

-- Function to find the next prime number
nextPrime :: Int -> Int
nextPrime n = nextPrimeHelper n (n + 1)

nextPrimeHelper :: Int -> Int -> Int
nextPrimeHelper base candidate
  | isPrime candidate = candidate
  | otherwise         = nextPrimeHelper base (candidate + 1)

main :: IO ()
main = do
  putStrLn "Enter a number: "
  number <- readLn :: IO Int
  -- Perform the multiplication before converting to string
  let result = "The number times two is: " ++ show (number * 2)
  putStrLn result



