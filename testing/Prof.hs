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

module Prof where

import Control.Monad
import Control.Monad.State
import Control.Applicative((<$>),(<*>))
import Control.Arrow
import Text.Printf.TH
import Data.Maybe
import Data.List as L
import qualified Data.ByteString as B
import Data.Function
import System.Process
import System.UTF8IO
import Control.Arrow
import Debug.Trace
import Prelude()
import UTF8Prelude hiding(catch)
import System.SimpleArgs
import Data.Generics
import Test.QuickCheck
import Test.QuickCheck.Gen
import Data.Monoid
import Text.PrettyPrint.ANSI.Leijen
import System.Exit
import Text.Regex.PCRE.Light.Char8
import System.Random
    
import "logic-TPTP" Codec.TPTP
    
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
