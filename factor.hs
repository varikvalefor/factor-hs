main :: IO ();
main = getLine >>= prt . read >> main
  where
  prt :: Integer -> IO ()
  prt n
    | product (primeFactors n) /= n = error "primeFactors is wrong!!!"
    | otherwise = putStrLn $ concat [show n, ": ", unwords (map show $ primeFactors n)];

primeFactors :: Integer -> [Integer];
primeFactors n
  | f == [n] = f
  | otherwise = concatMap primeFactors f
  where
  f :: [Integer]
  f = factors n;

factors :: Integer -> [Integer]
factors n
  | f == [1] = [n]
  | otherwise = [f !! 1, n `div` (f !! 1)]
  where
  f :: [Integer]
  f = filter ((==0) . (n `mod`)) [1..isqrt n];

isqrt :: Integer -> Integer;
isqrt n 
  | n <= fromIntegral (maxBound :: Int) = floor $ sqrt $ fromIntegral n + 0.0
  | otherwise = head $ filter ((>= n) . (^2)) [1..];
  
