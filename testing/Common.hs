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

module Common where

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
    
import "logic-TPTP" Codec.TPTP


data AFormulaComparison = OtherSame | OtherDiff String String | FormulaDiff (F DiffResult)
                        
instance Monoid AFormulaComparison where
    mempty = OtherSame
             
    -- keep the most interesting comparison result
    mappend OtherSame y = y
    mappend x OtherSame = x
    mappend x@(OtherDiff _ _) y@(FormulaDiff (F y0)) = if isSame y0 then y else x
    mappend x@(FormulaDiff (F x0)) y@(OtherDiff _ _) = if isSame x0 then x else y 
    mappend x@(OtherDiff _ _) y@(OtherDiff _ _) = x
    mappend x@(FormulaDiff _ ) y@(FormulaDiff _ ) = x
                                                                
instance Pretty AFormulaComparison where
    pretty (OtherSame) = dullgreen.text$"OtherSame"
    pretty (OtherDiff x y) = sep 
                             [dullred.text$"OtherDiff"
                             ,pretty x
                             ,pretty y
                             ]
    pretty (FormulaDiff fd) = pretty fd
     
compareOther x y = if x==y then OtherSame else OtherDiff (show x) (show y) 

diffAFormula (AFormula a b c d) (AFormula a1 b1 c1 d1) =
    mconcat [ compareOther a a1
            , compareOther b b1
            , FormulaDiff (diff c c1)
            , compareOther d d1
            ]
diffAFormula x y = compareOther x y 
                  
isThf = let re = compile "thf\\(" [] in (\x -> match re x [] /= Nothing)
