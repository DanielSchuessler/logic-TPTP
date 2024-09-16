{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module Common where

import Data.Monoid
import qualified Data.Semigroup as Semigroup
import Prettyprinter
import Prettyprinter.Render.Terminal
import Text.Regex.PCRE.Light.Char8
import Codec.TPTP


data AFormulaComparison = OtherSame | OtherDiff String String | FormulaDiff (F DiffResult)

instance Semigroup.Semigroup AFormulaComparison where
    -- keep the most interesting comparison result
    (<>) OtherSame y = y
    (<>) x OtherSame = x
    (<>) x@(OtherDiff _ _) y@(FormulaDiff (F y0)) = if isSame y0 then y else x
    (<>) x@(FormulaDiff (F x0)) y@(OtherDiff _ _) = if isSame x0 then x else y
    (<>) x@(OtherDiff _ _) (OtherDiff _ _) = x
    (<>) x@(FormulaDiff _ ) (FormulaDiff _ ) = x

instance Monoid AFormulaComparison where
    mempty = OtherSame
#if !MIN_VERSION_base(4,11,0)
    mappend = (Semigroup.<>)
#endif

instance PrettyAnsi AFormulaComparison where
    prettyAnsi (OtherSame) = annotate (colorDull Green).pretty$"OtherSame"
    prettyAnsi (OtherDiff x y) = sep
                             [annotate (colorDull Red).pretty$"OtherDiff"
                             ,pretty x
                             ,pretty y
                             ]
    prettyAnsi (FormulaDiff fd) = prettyAnsi fd

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
