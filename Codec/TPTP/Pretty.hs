{-# LANGUAGE NoMonomorphismRestriction, RecordWildCards
  , StandaloneDeriving
  , TypeSynonymInstances, FlexibleInstances, FlexibleContexts
  , UndecidableInstances, DeriveDataTypeable, GeneralizedNewtypeDeriving
  , OverlappingInstances, RankNTypes, PatternGuards
  #-}

{-# OPTIONS -Wall -fno-warn-orphans #-}

-- | Mainly just 'Pretty' instances
module Codec.TPTP.Pretty(prettySimple,WithEnclosing(..),Enclosing(..)) where

import Codec.TPTP.Base
import Codec.TPTP.Export
import Text.PrettyPrint.ANSI.Leijen
import Data.Data
import Control.Monad.Identity
import Data.Ratio

oper :: String -> Doc
oper = dullyellow . text
psym :: forall a. (Pretty a) => a -> Doc
psym = green . pretty
fsym :: forall a. (Pretty a) => a -> Doc
fsym = yellow . pretty

--wtext :: String -> Doc
--wtext = white . text

prettyargs :: forall a. (Pretty a) => [a] -> Doc
--prettyargs = encloseSep (wtext "(") (wtext ")") (wtext ",") . fmap pretty
prettyargs = tupled . fmap pretty
-- prettyargs [] = white $ text "()"
-- prettyargs (x:xs) =
--     let pxs = fmap ((white (text ",") <+>) . pretty) xs in

--     align (fillSep $ [    white (text "(") <+> pretty x ]
--                     ++ pxs
--                     ++ [ white (text ")") ] )

prational :: Rational -> Doc
prational q = (text.show.numerator) q <> char '/' <> (text.show.denominator) q

-- | Carries information about the enclosing operation (for the purpose of printing stuff without parentheses if possible).
data WithEnclosing a = WithEnclosing Enclosing a

data Enclosing = EnclBinOp BinOp | EnclQuant | EnclNeg | EnclInfixPred InfixPred | EnclNothing
               deriving (Eq,Ord,Show,Data,Typeable,Read)

unaryEncl :: Enclosing -> Bool
unaryEncl EnclNeg = True
unaryEncl EnclQuant = True
unaryEncl _ = False

needsParens :: Enclosing -> Enclosing -> Bool
needsParens inner outer =
    case (inner,outer) of
      -- special handling for associative ops
      ((EnclBinOp (:&:)),(EnclBinOp (:&:))) -> False
      ((EnclBinOp (:|:)),(EnclBinOp (:|:))) -> False
      ((EnclBinOp (:<~>:)),(EnclBinOp (:<~>:))) -> False

      -- chains of quantifiers/negation don't need parentheses
      _
          | unaryEncl inner, unaryEncl outer
                -> False

      _ -> prec inner <= prec outer
  where
    prec :: Enclosing -> Int
    prec x = case x of
               (EnclInfixPred _) -> 60
               (EnclNeg) -> 50
               (EnclQuant) -> 50
               (EnclBinOp (:&:)) -> 40
               (EnclBinOp (:~&:)) -> 40
               (EnclBinOp (:|:)) -> 30
               (EnclBinOp (:~|:)) -> 30
               (EnclBinOp (:=>:)) -> 20
               (EnclBinOp (:<=:)) -> 20
               (EnclBinOp (:<=>:)) -> 20
               (EnclBinOp (:<~>:)) -> 20
               EnclNothing -> (-1000) -- if there's no enclosing operation, we don't need parentheses


maybeParens :: Enclosing -> Enclosing -> Doc -> Doc
maybeParens inner outer | needsParens inner outer = parens
maybeParens _ _ = id

instance Pretty TPTP_Input where
    pretty AFormula{..}
        = (text) "Formula " <+> (dullwhite.text) "name:"
          </>
          vsep
          [
            (red.pretty) name <+> dullwhite (text "role:") <+> (magenta.text.unrole) role
          , pretty formula
          , pretty annotations
          ,empty
          ]

    pretty (Comment str)
        = magenta . string $ str

    pretty (Include f sel) = text "include" <> parens (squotes (text f) <> (case sel of { [] -> empty; _ -> comma <+> sep (punctuate comma (fmap pretty sel)) } ))


    prettyList = foldr (\x xs -> pretty x <$> xs) empty

instance Pretty Quant where
    pretty All = oper "∀"
    pretty Exists = oper "∃"

instance (Pretty (WithEnclosing t), Pretty (WithEnclosing f)) => Pretty ((WithEnclosing (Formula0 t f))) where
    pretty (WithEnclosing enclosing formu) =
        let
            newEnclosing =
             case formu of
               Quant _ _ _ -> EnclQuant
               PredApp _ _ -> EnclNothing -- arguments are comma-seperated, so they don't need any parens
               (:~:) _ -> EnclNeg
               BinOp _ op _ -> EnclBinOp op
               InfixPred _ op _ -> EnclInfixPred op

            wne = WithEnclosing newEnclosing

        in
          maybeParens newEnclosing enclosing $

          case formu of

            Quant q vars f ->
               pretty q
               <+> brackets (hsep (punctuate comma (fmap pretty vars)))
               <> dot
               </> pretty (wne f)

            (:~:) f -> oper "¬" <+> pretty (wne f)

            PredApp p [] -> psym p
            PredApp p args -> psym p <> prettyargs (fmap wne args)


            BinOp f1 op f2 ->
                align $ sep [indent 0 $ pretty (wne f1), pretty op, indent 0 $ pretty (wne f2)]

            InfixPred f1 op f2 ->
                align $ sep [indent 0 $ pretty (wne f1), pretty op, indent 0 $ pretty (wne f2)]

instance Pretty BinOp where
    pretty x = case x of
        (:<=>:) -> oper "⇔"
        (:=>:)  -> oper "⇒"
        (:<=:)  -> oper "⇐"
        (:&:)   -> oper "∧"
        (:|:)   -> oper "∨"
        (:~&:)  -> oper "NAND"
        (:~|:)  -> oper "NOR"
        (:<~>:) -> oper "XOR"

instance Pretty InfixPred where
    pretty x = case x of
        (:=:)   -> oper "="
        (:!=:)  -> oper "≠"

instance (Pretty (WithEnclosing t)) => Pretty (WithEnclosing (Term0 t)) where
    pretty (WithEnclosing _ x) =
        case x of
          Var s -> pretty s
          NumberLitTerm d -> prational d
          DistinctObjectTerm s -> cyan (dquotes (text s))
          FunApp f [] -> fsym f
          FunApp f args -> fsym f <> prettyargs (fmap (WithEnclosing EnclNothing) args)



instance Pretty (Formula0 Term Formula) where
    pretty = pretty . WithEnclosing EnclNothing . F . Identity

instance Pretty (Term0 Term) where
    pretty = pretty . WithEnclosing EnclNothing . T . Identity

instance Pretty (WithEnclosing Formula) where
    pretty (WithEnclosing x (F (Identity y))) = pretty (WithEnclosing x y)

instance Pretty (WithEnclosing Term) where
    pretty (WithEnclosing x (T (Identity y))) = pretty (WithEnclosing x y)

deriving instance Pretty Formula
deriving instance Pretty Term
deriving instance Pretty a => Pretty (Identity a)

-- instance (Pretty f, Pretty t) => Pretty (Formula0 t f) where
--     pretty = pretty . WithEnclosing ""

-- instance (Pretty t) => Pretty (Term0 t) where
--     pretty = pretty . WithEnclosing ""

prettySimple :: Pretty a => a -> String
prettySimple x = displayS (renderPretty 0.9 80 (pretty x)) ""

instance Pretty Annotations where
    pretty NoAnnotations = dullwhite . text $ "NoAnnotations"
    pretty (Annotations a b) = dullwhite (text "SourceInfo: ") <+> pretty a <+> pretty b

instance Pretty UsefulInfo where
    pretty NoUsefulInfo = empty
    pretty (UsefulInfo x) = dullwhite (text "UsefulInfo: ") <+> pretty x



instance Pretty GData where
    pretty (GWord x) = pretty x
    pretty (GNumber x) = prational x
    pretty (GDistinctObject x) = cyan (dquotes (text x))
    pretty (GApp x []) = fsym x
    pretty (GApp x args) = fsym x <+> prettyargs args
    pretty (GFormulaData s f) = text s <> align (parens (pretty f))
    pretty (GVar x) = pretty x

instance Pretty AtomicWord where
    pretty (AtomicWord x) = (if isLowerWord x then text else squotes.text) x

instance Pretty GTerm where
    pretty (GTerm x) = pretty x
    pretty (ColonSep x y) = pretty x <+> oper ":" <+> pretty y
    pretty (GList xs) = let f = oper
                        in
                          f "[" <+> (fillSep . punctuate comma . fmap pretty) xs <+> f "]"



instance Pretty V where
    pretty (V x) = blue . text $ x
