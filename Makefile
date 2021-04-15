build: genprimes.hs factor.hs
        ghc -O2 genprimes
        echo "module ThePrimes where\n  thePrimes :: [Integer];\n  thePrimes = [`./genprimes 6`];" > ThePrimes.hs
        ghc -O2 factor
delcompile:
        rm *.hi *.o
        rm genprimes
