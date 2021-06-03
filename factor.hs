import ThePrimes;

-- | For all Integer k, printFactors k prints the space-separated
-- prime factors of k to the terminal.
printFactors :: Integer -> IO ();
printFactors k = putStr (show k ++ ": ") >> showNums
  where
    listFacs = filter ((==0) . mod k) thePrimes
    showNums = mapM_ (putStr . (++ " ") . show) listFacs >> putStrLn "";

-- | main is the main program loop.
-- For all input String k, if k can be parsed as a value of type Integer,
-- then the factors of the Integer version of k are printed to the
-- terminal.
main :: IO ();
main = getLine >>= printFactors . read >> main;
