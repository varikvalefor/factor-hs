import ThePrimes;

main :: IO ();
main = getLine >>= prt . read >> main
  where
  prt :: Integer -> IO ()
  prt n
    | product (primeFactors n) /= n = error "primeFactors is wrong!!!"
    | otherwise = putStrLn $ concat [show n, ": ", unwords (map show $ primeFactors n)];

-- | @primeFactors k@ is a list of numbers @j@ such that
-- @product j == k@ and @j@ is a subset of the prime numbers.
primeFactors :: Integer -> [Integer];
primeFactors n
  | f == [n] = f
  | otherwise = concatMap primeFactors f
  where
  f :: [Integer]
  f = factors n;

-- | If @n@ is a composite number, then @factors n@ is a 2-list whose
-- product equals @n@.
--
-- If @n@ is prime, then @factors n == [n]@.
factors :: Integer -> [Integer]
factors n
  | f == [1] = [n]
  | otherwise = [f !! 1, n `div` (f !! 1)]
  where
  f :: [Integer]
  f = filter ((==0) . (n `mod`)) [1..isqrt n];

-- | @isqrt n@ is the integer square root of @n@.
isqrt :: Integer -> Integer;
isqrt n 
  | n <= fromIntegral (maxBound :: Int) = floor $ sqrt $ fromIntegral n + 0.0
  | otherwise = head $ filter ((>= n) . (^2)) [1..];
  
