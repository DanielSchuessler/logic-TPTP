{-# LANGUAGE NoMonomorphismRestriction, RecordWildCards
  , StandaloneDeriving
  , TypeSynonymInstances, FlexibleInstances, FlexibleContexts
  , UndecidableInstances, DeriveDataTypeable, GeneralizedNewtypeDeriving
  , OverlappingInstances, RankNTypes, PatternGuards
  #-}
{-# OPTIONS -Wall #-}
module Codec.TPTP.QuickCheck where
    
import Test.QuickCheck
import Control.Monad
import Control.Applicative
import Data.Char
import Data.Array.ST
import Data.Array.IArray
import Data.Array.Base

argsFreq :: (Int -> Gen a) -> Gen a
argsFreq f = frequency [ (10,f 0)
                        , (10,f 1)
                        , (10,f 2)
                        , (2 ,f 3)
                        , (2 ,f 4)
                        , (1, f 15)
                        ]
              
arbVar :: Gen [Char]
arbVar = liftM2 (:) (elements "WXZY") (show <$> choose(1::Int,3))  
        
-- sorry I don't feel like calculating this distribution deductively at the moment ;)
-- | Return random partition of n items into the given number of buckets if buckets>0
--
-- Return [] if buckets=0
arbPartition :: Int -> Int -> Gen [Int]
arbPartition 0 _ = return []
arbPartition 1 n = return [n]
arbPartition buckets n = do
  choices <- replicateM n (choose (1,buckets))
  let uarray :: UArray Int Int
      uarray = runSTUArray $
        do arrr <- newArray (1,buckets) 0
           forM_ choices (\bucket -> writeArray arrr bucket . succ =<< readArray arrr bucket)
           return arrr
  return $ elems uarray 

arbPrintable :: Gen [Char]
arbPrintable = listOf (arbitrary `suchThat` printable)
               
printable :: Char -> Bool
printable x = isAscii x && isPrint x --isDigit x || isLetter x || x `elem` " _+!@#$%^&*()[]{}-=?.,;'\":/\\"
    
arbLowerWord :: Gen String
arbLowerWord = (:) `fmap` elements ['a'..'z'] `ap` listOf (elements (['a'..'z']++['A'..'Z']++"_"))


arbUpperWord :: Gen String
arbUpperWord = (:) `fmap` elements ['A'..'Z'] `ap` listOf (elements (['a'..'z']++['A'..'Z']++"_"))

               
arbNum :: forall a a1. (Arbitrary a, Num a) => (a -> a1) -> Gen a1
arbNum f =
    frequency [(1,fmap f arbitrary)
              ,(8, fmap (f . fromIntegral) (arbitrary::Gen Int))
              ]
