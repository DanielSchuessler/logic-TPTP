{-# OPTIONS_GHC -Wall #-}
module Main where

import Test.QuickCheck
import Common
import Data.Functor.Identity

import Codec.TPTP

main ::  IO ()
main = quickCheckWith (stdArgs { maxSuccess = 5000 }) prop_test_ie

prop_test_ie :: TPTP_Input_ Identity -> Property
prop_test_ie f =
    let tptp = toTPTP' [f] in

      (let
          [g] = parse tptp -- $ trace tptp tptp

          dif = diffAFormula f g
       in
          counterexample tptp $
           whenFail (putStrLn . prettySimple $ dif) $
            (f==g)

          -- (case dif of
          --    OtherSame -> True
          --    FormulaDiff (F Same) -> True
          --    _ -> False
          -- ))

      )
