import ThePrimes;

reed :: String -> Integer;
reed k = read k :: Integer;

printFactors :: Integer -> IO ();
printFactors k = putStr (show k ++ ": ") >> showTheNums
  where
    listNums = filter (\a -> mod k a == 0) thePrimes
    showTheNums = mapM_ (\ a -> putStr $ show a ++ " ") listNums >> putStrLn "";

main :: IO ();
main = getLine >>= printFactors . reed >> main;
