build: genprimes.hs factor.hs
        ghc -O2 factor
delcompile:
        rm *.hi *.o
        rm genprimes
