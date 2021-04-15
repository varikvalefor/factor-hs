import ThePrimes;

reed :: String -> Integer;
reed k = read k :: Integer;

printFactors :: Integer -> IO ();
printFactors k = putStr (show k ++ ": ") >> showNums
  where
    listNums = filter ((==0) . mod k) thePrimes
    showNums = mapM_ (putStr . (++ " ") . show) listNums >> putStrLn "";

main :: IO ();
main = getLine >>= printFactors . reed >> main;
