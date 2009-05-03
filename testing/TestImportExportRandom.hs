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

module TestImportExportRandom where

import Control.Monad
import Control.Monad.State
import Control.Applicative((<$>),(<*>))
import Control.Arrow
import Text.Printf.TH
import Data.Maybe
import Data.List as L
import Data.Map as M
import Data.Set as S
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
import Data.Monoid
import Text.PrettyPrint.ANSI.Leijen
import System.Exit
import Text.Regex.PCRE.Light.Char8
import Common
    
import "logic-TPTP" Codec.TPTP

main = quickCheckWith (stdArgs { maxSuccess = 5000 }) prop_test_ie

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
