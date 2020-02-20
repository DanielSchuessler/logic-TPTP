{-# OPTIONS
 -fglasgow-exts
 -XCPP
 -XTemplateHaskell
 -XNamedFieldPuns
 -XRecordWildCards
 -XDeriveDataTypeable
 -XOverlappingInstances
 -XPackageImports
 -fwarn-incomplete-patterns
 #-}

module Main where

import Control.Monad.State(Monad(..), Functor(..), (=<<),
                           replicateM, unless)
import Codec.TPTP(TPTP_Input, parse, toTPTP')
import Test.QuickCheck(Arbitrary(arbitrary))
import Test.QuickCheck.Gen(Gen(unGen))
import System.SimpleArgs(Args(..))
import System.Random(newStdGen)


size = 100

randomF :: IO TPTP_Input
randomF = (\seed -> unGen arbitrary seed size) `fmap` newStdGen

main = do
  reps <- getArgs
  print . and =<< replicateM reps (do
     f <- randomF
     let res = checkit f
     -- unless res (print f >> replicateM 20 (putChar '-') >> print (parse(toTPTP' [f])))
     unless res (writeFile "tmp1" (format [f]) >> writeFile "tmp2" (format (parse (toTPTP' [f]))))
     return res
                                  )

checkit :: TPTP_Input -> Bool
checkit f = [f] == parse (toTPTP' [f])

format = unlines . words . show
