{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

module Common where

import Control.Monad
import Data.List as L
import Data.Function
import Data.Monoid
import Text.PrettyPrint.ANSI.Leijen
import Text.Regex.PCRE.Light.Char8
import Codec.TPTP


data AFormulaComparison = OtherSame | OtherDiff String String | FormulaDiff (F DiffResult)
                        
instance Monoid AFormulaComparison where
    mempty = OtherSame
             
    -- keep the most interesting comparison result
    mappend OtherSame y = y
    mappend x OtherSame = x
    mappend x@(OtherDiff _ _) y@(FormulaDiff (F y0)) = if isSame y0 then y else x
    mappend x@(FormulaDiff (F x0)) y@(OtherDiff _ _) = if isSame x0 then x else y 
    mappend x@(OtherDiff _ _) (OtherDiff _ _) = x
    mappend x@(FormulaDiff _ ) (FormulaDiff _ ) = x
                                                                
instance Pretty AFormulaComparison where
    pretty (OtherSame) = dullgreen.text$"OtherSame"
    pretty (OtherDiff x y) = sep 
                             [dullred.text$"OtherDiff"
                             ,pretty x
                             ,pretty y
                             ]
    pretty (FormulaDiff fd) = pretty fd
     
compareOther ::  (Eq a, Show a) => a -> a -> AFormulaComparison
compareOther x y = if x==y then OtherSame else OtherDiff (show x) (show y) 

diffAFormula :: (Eq (t (Formula0 (T t) (F t))),Show (t (Formula0 (T t) (F t))),Diffable (F t) (F DiffResult)) =>TPTP_Input_ t -> TPTP_Input_ t -> AFormulaComparison
diffAFormula (AFormula a b c d) (AFormula a1 b1 c1 d1) =
    mconcat [ compareOther a a1
            , compareOther b b1
            , FormulaDiff (diff c c1)
            , compareOther d d1
            ]
diffAFormula x y = compareOther x y 
                  
findUnsupportedFormulaType ::  String -> Maybe String
findUnsupportedFormulaType = 
    let re = compile "^(thf|tff)\\(" [multiline] 
    in (\x -> (!!1) `fmap` match re x [])
