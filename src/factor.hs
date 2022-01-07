import Data.List.Extra;

main :: IO ();
main = getLine >>= prt . read >> main
  where
  prt :: Integer -> IO ()
  prt n
    | product (primeFactors n) /= n = print (primeFactors n) >> error "primeFactors is wrong!!!"
    | otherwise = putStrLn $ concat [show n, ": ", unwords $ map show $ primeFactors n];

-- | @primeFactors k@ is a list of numbers @j@ such that
-- @product j == k@ and @j@ is a subset of the prime numbers.
primeFactors :: Integer -> [Integer];
primeFactors n = maybe [n] (concatMap primeFactors) $ factors n;

-- | If @n@ is a composite number, then @factors n@ is 'Just' a 2-list
-- whose product equals @n@.
--
-- If @n@ is prime, then @factors n == 'Nothing'@.
factors :: Integer -> Maybe [Integer]
factors n = (\d -> [d, n `div` d]) <$> firstNonUnityFactor n;

-- \| If @n@ is prime, then @firstNonUnityFactor@ is 'Nothing'.
--
-- If @n@ is composite, then @firstNonUnityFactor n@ is the smallest
-- non-1 divisor of @n@.
firstNonUnityFactor :: Integer -> Maybe Integer;
firstNonUnityFactor n = allFactors !? 1
  where
  allFactors :: [Integer]
  allFactors = filter ((==0) . (n `mod`)) [1..isqrt n];

-- | @isqrt n@ is the integer square root of @n@.
isqrt :: Integer -> Integer;
isqrt n 
  | n <= fromIntegral (maxBound :: Int) = floor $ sqrt $ fromIntegral n
  | otherwise = head $ filter ((>= n) . (^2)) [1..];
