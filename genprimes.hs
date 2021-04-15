import System.Environment
import Data.List;

isPrime :: Integer -> Bool;
isPrime k = not $ any ((== 0) . mod k) [2..squirt k - 1];

squirt :: Integer -> Integer;
squirt = floor . sqrt . fromIntegral;

main :: IO ();
main = getArgs >>= return . (!!0) >>= \ a ->
  mapM_ putStr $ intersperse "," $ map show $ filter isPrime [2..10^(read a)];
