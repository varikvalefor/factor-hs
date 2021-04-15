import ThePrimes;

reed :: String -> Integer;
reed = read;

printFactors :: Integer -> IO ();
printFactors k = putStr (show k ++ ": ") >> showNums
  where
    listFacs = filter ((==0) . mod k) thePrimes
    showNums = mapM_ (putStr . (++ " ") . show) listFacs >> putStrLn "";

main :: IO ();
main = getLine >>= printFactors . reed >> main;
