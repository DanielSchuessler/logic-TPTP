{-# OPTIONS -Wall #-}
{-# LANGUAGE PackageImports #-}
module Main where

import Data.Function
import System.IO
import Test.QuickCheck
import Common
import Data.Functor.Identity

import "logic-TPTP" Codec.TPTP

main ::  IO ()
main = quickCheckWith (stdArgs { maxSuccess = 5000 }) prop_test_ie

prop_test_ie :: TPTP_Input_ Identity -> Property
prop_test_ie f =
    let tptp = toTPTP' [f] in

      (let
          [g] = parse tptp -- $ trace tptp tptp

          dif = diffAFormula f g
       in
          whenFail
          (putStrLn . prettySimple $ dif)

           (f==g)

          -- (case dif of
          --    OtherSame -> True
          --    FormulaDiff (F Same) -> True
          --    _ -> False
          -- ))

      )
