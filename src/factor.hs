import Data.List.Extra;

main :: IO ();
main = getLine >>= prt . read >> main
  where
  prt :: Integer -> IO ()
  prt n
    | product (primeFactors n) /= n = wrongRoutine n
    | otherwise = putStrLn $ concat [show n, ": ", theFactors n]
  theFactors n = unwords $ map show $ primeFactors n
  wrongRoutine n = print (primeFactors n) >> error wrongMsg
  wrongMsg = "primeFactors is wrong!!!";

-- | @primeFactors k@ is a list of primes @j@ such that
-- @product j == k@.
primeFactors :: Integer -> [Integer];
primeFactors n = maybe [n] (concatMap primeFactors) $ factors n;

-- | If @n@ is a composite number, then @factors n@ is 'Just' a 2-list
-- whose product equals @n@.
--
-- If @n@ is prime, then @factors n == 'Nothing'@.
factors :: Integer -> Maybe [Integer]
factors n = (\d -> [d, n `div` d]) <$> firstNonUnityFactor n;

-- | If @n@ is prime, then @firstNonUnityFactor n@ is 'Nothing'.
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
isqrt n = helpy 0 $ n + 1
  where
  helpy :: Integer -> Integer -> Integer
  helpy lowBound highBound
    | lowBound == highBound - 1 = lowBound
    | newBound^2 > n = helpy lowBound newBound
    | otherwise = helpy newBound highBound
    where newBound = (highBound + lowBound) `div` 2;
