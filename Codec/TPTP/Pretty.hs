{-# LANGUAGE CPP, NoMonomorphismRestriction, RecordWildCards
  , StandaloneDeriving
  , TypeSynonymInstances, FlexibleInstances, FlexibleContexts
  , UndecidableInstances, DeriveDataTypeable, GeneralizedNewtypeDeriving
  , OverlappingInstances, RankNTypes, PatternGuards
  #-}

{-# OPTIONS -Wall -fno-warn-orphans #-}

-- | Mainly just 'Pretty' instances
module Codec.TPTP.Pretty(PrettyAnsi(..),prettySimple,WithEnclosing(..),Enclosing(..)) where

#if MIN_VERSION_base(4,8,0)
import Prelude hiding ((<$>))
#endif

import Codec.TPTP.Base
import Codec.TPTP.Export
import Prettyprinter
import Prettyprinter.Render.Terminal
import Data.Data
import Control.Monad.Identity
import Data.Ratio
import qualified Data.Text.Lazy as TL

class PrettyAnsi a where
  prettyAnsi :: a -> Doc AnsiStyle
  prettyAnsiList :: [a] -> Doc AnsiStyle
  prettyAnsiList = align . list . map prettyAnsi

instance PrettyAnsi (Doc AnsiStyle) where
  prettyAnsi = id

instance PrettyAnsi a => PrettyAnsi [a] where
  prettyAnsi = prettyAnsiList

oper :: String -> Doc AnsiStyle
oper = annotate (colorDull Yellow) . text
psym :: forall a. (Pretty a) => a -> Doc AnsiStyle
psym = annotate (color Green) . pretty
fsym :: forall a. (Pretty a) => a -> Doc AnsiStyle
fsym = annotate (color Yellow) . pretty

text :: String -> Doc ann
text = pretty

--wtext :: String -> Doc
--wtext = annotate (color White) . text

prettyargs :: forall a. (PrettyAnsi a) => [a] -> Doc AnsiStyle
--prettyargs = encloseSep (wtext "(") (wtext ")") (wtext ",") . fmap pretty
prettyargs = tupled . fmap prettyAnsi
-- prettyargs [] = annotate (color White) $ text "()"
-- prettyargs (x:xs) =
--     let pxs = fmap ((annotate (color White) (text ",") <+>) . pretty) xs in

--     align (fillSep $ [    annotate (color White) (text "(") <+> pretty x ]
--                     ++ pxs
--                     ++ [ annotate (color White) (text ")") ] )

prational :: Rational -> Doc ann
prational q = (text.show.numerator) q <> pretty '/' <> (text.show.denominator) q

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


maybeParens :: Enclosing -> Enclosing -> Doc ann -> Doc ann
maybeParens inner outer | needsParens inner outer = parens
maybeParens _ _ = id

instance PrettyAnsi TPTP_Input where
    prettyAnsi AFormula{..}
        = fillSep
          [ (text) "Formula " <+> (annotate (colorDull White).text) "name:"
          , vsep
            [
              (annotate (color Red).pretty) name <+> annotate (colorDull White) (text "role:") <+> (annotate (color Magenta).text.unrole) role
            , prettyAnsi formula
            , prettyAnsi annotations
            , mempty
            ]
          ]

    prettyAnsi (Comment str)
        = annotate (color Magenta) . pretty $ str

    prettyAnsi (Include f sel) = text "include" <> parens (squotes (text f) <> (case sel of { [] -> mempty; _ -> comma <+> sep (punctuate comma (fmap pretty sel)) } ))


    prettyAnsiList = foldr (\x xs -> vsep [prettyAnsi x, xs]) mempty

instance PrettyAnsi Quant where
    prettyAnsi All = oper "∀"
    prettyAnsi Exists = oper "∃"

instance (PrettyAnsi (WithEnclosing t), PrettyAnsi (WithEnclosing f)) => PrettyAnsi ((WithEnclosing (Formula0 t f))) where
    prettyAnsi (WithEnclosing enclosing formu) =
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
               fillSep
               [ prettyAnsi q
                 <+> brackets (hsep (punctuate comma (fmap prettyAnsi vars)))
                 <> dot
               , prettyAnsi (wne f)
               ]

            (:~:) f -> oper "¬" <+> prettyAnsi (wne f)

            PredApp p [] -> psym p
            PredApp p args -> psym p <> prettyargs (fmap wne args)


            BinOp f1 op f2 ->
                align $ sep [indent 0 $ prettyAnsi (wne f1), prettyAnsi op, indent 0 $ prettyAnsi (wne f2)]

            InfixPred f1 op f2 ->
                align $ sep [indent 0 $ prettyAnsi (wne f1), prettyAnsi op, indent 0 $ prettyAnsi (wne f2)]

instance PrettyAnsi BinOp where
    prettyAnsi x = case x of
        (:<=>:) -> oper "⇔"
        (:=>:)  -> oper "⇒"
        (:<=:)  -> oper "⇐"
        (:&:)   -> oper "∧"
        (:|:)   -> oper "∨"
        (:~&:)  -> oper "NAND"
        (:~|:)  -> oper "NOR"
        (:<~>:) -> oper "XOR"

instance PrettyAnsi InfixPred where
    prettyAnsi x = case x of
        (:=:)   -> oper "="
        (:!=:)  -> oper "≠"

instance (PrettyAnsi (WithEnclosing t)) => PrettyAnsi (WithEnclosing (Term0 t)) where
    prettyAnsi (WithEnclosing _ x) =
        case x of
          Var s -> prettyAnsi s
          NumberLitTerm d -> prational d
          DistinctObjectTerm s -> annotate (color Cyan) (dquotes (text s))
          FunApp f [] -> fsym f
          FunApp f args -> fsym f <> prettyargs (fmap (WithEnclosing EnclNothing) args)



instance PrettyAnsi (Formula0 Term Formula) where
    prettyAnsi = prettyAnsi . WithEnclosing EnclNothing . F . Identity

instance PrettyAnsi (Term0 Term) where
    prettyAnsi = prettyAnsi . WithEnclosing EnclNothing . T . Identity

instance PrettyAnsi (WithEnclosing Formula) where
    prettyAnsi (WithEnclosing x (F (Identity y))) = prettyAnsi (WithEnclosing x y)

instance PrettyAnsi (WithEnclosing Term) where
    prettyAnsi (WithEnclosing x (T (Identity y))) = prettyAnsi (WithEnclosing x y)

deriving instance PrettyAnsi Formula
deriving instance PrettyAnsi Term
deriving instance PrettyAnsi a => PrettyAnsi (Identity a)

-- instance (Pretty f, Pretty t) => PrettyAnsi (Formula0 t f) where
--     pretty = pretty . WithEnclosing ""

-- instance (Pretty t) => Pretty (Term0 t) where
--     pretty = pretty . WithEnclosing ""

prettySimple :: PrettyAnsi a => a -> String
prettySimple x = TL.unpack $ renderLazy $ layoutSmart LayoutOptions{ layoutPageWidth = AvailablePerLine 80 0.9 } $ prettyAnsi x


instance PrettyAnsi Annotations where
    prettyAnsi NoAnnotations = annotate (colorDull White) . text $ "NoAnnotations"
    prettyAnsi (Annotations a b) = annotate (colorDull White) (text "SourceInfo: ") <+> prettyAnsi a <+> prettyAnsi b

instance PrettyAnsi UsefulInfo where
    prettyAnsi NoUsefulInfo = mempty
    prettyAnsi (UsefulInfo x) = annotate (colorDull White) (text "UsefulInfo: ") <+> prettyAnsi x



instance PrettyAnsi GData where
    prettyAnsi (GWord x) = pretty x
    prettyAnsi (GNumber x) = prational x
    prettyAnsi (GDistinctObject x) = annotate (color Cyan) (dquotes (text x))
    prettyAnsi (GApp x []) = fsym x
    prettyAnsi (GApp x args) = fsym x <+> prettyargs args
    prettyAnsi (GFormulaData s f) = text s <> align (parens (prettyAnsi f))
    prettyAnsi (GFormulaTerm s t) = text s <> align (parens (prettyAnsi t))
    prettyAnsi (GVar x) = prettyAnsi x

instance Pretty AtomicWord where
    pretty (AtomicWord x) = (if isLowerWord x then text else squotes.text) x

instance PrettyAnsi GTerm where
    prettyAnsi (GTerm x) = prettyAnsi x
    prettyAnsi (ColonSep x y) = prettyAnsi x <+> oper ":" <+> prettyAnsi y
    prettyAnsi (GList xs) = let f = oper
                            in
                              f "[" <+> (fillSep . punctuate comma . fmap prettyAnsi) xs <+> f "]"



instance PrettyAnsi V where
    prettyAnsi (V x) = annotate (color Blue) . text $ x
