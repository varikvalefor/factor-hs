import Data.Bool;
import Data.List;
import Data.Maybe;
import Data.List.Extra;
import qualified Data.Set as S;

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

-- | @isPrime k@ iff @k@ is prime.
isPrime :: Integer -> Bool;
isPrime = isNothing . factors;

-- | If @n@ is prime, then @toFactorTriplet n@ is 'Nothing'.
-- @toFactorTriplet n@ is otherwise 'Just' a 3-tuple @(a,b,c)@
-- such that @a == n@ and @a == b * c@.
toFactorTriplet :: Integer -> Maybe (Integer, Integer, Integer);
toFactorTriplet n = (\[a,b] -> (n,a,b)) <$> factors n;

-- | Where \(k\) is any 'Integer' and \(t\) denotes
-- @toFactorTriplets k@...
--
-- \[
--   \forall u \in t,\ 
--   t_1 = t_2 \cdot t_3.
-- \]
--
-- \[
--   \max \left\{a_0 : a \in t\right\} = k.
-- \]
--
-- \[
--   \forall n \in t,\ 
--   \forall m \in \left\{2,3\right\},\ 
--   \left(
--     n_m \in \mathbb P
--   \right) \veebar
--   \left(
--     \left\{j \in t : j_1 = n_m\right\} \neq \left\{\right\}
--   \right).
-- \]
--
-- \[
--   \forall n \in t,\ 
--   \left\{1, k\right\} \cap t = \left\{\right\}.
-- \]
toFactorTriplets :: Integer -> S.Set (Integer, Integer, Integer);
toFactorTriplets = fl . recurse . catMaybes . pure . toFactorTriplet
  where
  fl = S.fromList
  recurse l = bool (recurse $ l ++ newTriplets l) l $ null $ newTriplets l
  newTriplets l = catMaybes $ map toFactorTriplet $ filter (\a -> filter (\(t,_,_) -> t == a) l == [] && not (isPrime a)) $ concat $ map (\(a,b,c) -> [b,c]) l

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
