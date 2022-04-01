module Main where
import Data.Bool;
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
--   u_1 = u_2 \cdot u_3.
-- \]
--
-- \[
--   \max \left\{a_1 : a \in t\right\} = k.
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
--   \left\{1, n_1\right\} \cap \left\{n_2, n_3\right\} =
--   \left\{\right\}.
-- \]
--
-- \[
--   \nexists a \in t \colon
--   \exists b \in t \setminus a \colon
--   \left\langle a_2, a_3\right\rangle =
--   \left\langle b_3,b_2\right\rangle.
-- \]
toFactorTriplets :: Integer -> S.Set (Integer, Integer, Integer);
toFactorTriplets = fl . recurse . catMaybes . pure . toFactorTriplet
  where
  fl = S.fromList
  recurse l = bool (recurse $ l ++ newTriplets) l $ null newTriplets
    where
    newTriplets = catMaybes $ map toFactorTriplet tbc
    isConvertible j = not $ alreadyCalculated j || isPrime j
    alreadyCalculated j = not $ null $ filter ((== j) . fst') l
    fst' (a,_,_) = a
    -- \| "@tbc@" is an abbreviation of "to be converted".
    tbc = filter isConvertible $ concat $ map (\(_,b,c) -> [b,c]) l;

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
