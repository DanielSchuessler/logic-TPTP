{-# LANGUAGE NoMonomorphismRestriction, RecordWildCards
  , StandaloneDeriving
  , TypeSynonymInstances, FlexibleInstances, FlexibleContexts
  , UndecidableInstances, DeriveDataTypeable, GeneralizedNewtypeDeriving
  , OverlappingInstances, RankNTypes
  #-}
{-# OPTIONS -Wall #-}
module Codec.TPTP.Export(toTPTP, toTPTP',ToTPTP(..),toTPTPByteString,isLowerWord) where

import Codec.TPTP.Base
import Control.Monad.Identity
import Data.List (intersperse)
import Data.Monoid hiding (All (..))
import Data.Ratio
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.Builder as Builder


toTPTP :: forall a. (ToTPTP a) => a -> ShowS
toTPTP a s = toTPTP' a ++ s

toTPTP' :: forall a. (ToTPTP a) => a -> String
toTPTP' = BL.unpack . toTPTPByteString -- TODO: handle UTF-8

toTPTPByteString :: forall a. (ToTPTP a) => a -> BL.ByteString
toTPTPByteString = Builder.toLazyByteString . toTPTPBuilder

s :: String -> Builder.Builder
s = Builder.string7

comma :: Builder.Builder
comma = Builder.char7 ','

commaSepMap :: forall a.
               (a -> Builder.Builder) -> [a] -> Builder.Builder
commaSepMap f = mconcat . intersperse comma . map f




class ToTPTP a where
    -- | Convert to TPTP
    toTPTPBuilder :: a -> Builder.Builder

instance ToTPTP [TPTP_Input] where
    toTPTPBuilder = mconcat . map (\x -> toTPTPBuilder x <> Builder.char8 '\n')

instance ToTPTP TPTP_Input where
    toTPTPBuilder AFormula{..} =
        s "fof(" <> toTPTPBuilder name <> comma <> toTPTPBuilder role <> comma <>
          toTPTPBuilder formula <> toTPTPBuilder annotations <> s ")."

    toTPTPBuilder (Comment x) =
        Builder.stringUtf8 x -- % included in x

    toTPTPBuilder (Include x sel) = s "include" <> s "(" <> Builder.stringUtf8 (tptpSQuote x) <>

                             case sel of { [] -> mempty; _ -> s ",[" <> commaSepMap toTPTPBuilder sel <> s "]" } <>


                             s ")."


instance ToTPTP Role where
    toTPTPBuilder (Role x) = Builder.stringUtf8 x

instance ToTPTP Quant where
    toTPTPBuilder All = s "!"
    toTPTPBuilder Exists = s "?"

instance ToTPTP InfixPred where
    toTPTPBuilder x = case x of
        (:=:)  -> s "="
        (:!=:) -> s "!="

instance ToTPTP BinOp where
    toTPTPBuilder x = case x of
        (:<=>:) -> s "<=>"
        (:=>:)  -> s "=>"
        (:<=:)  -> s "<="
        (:&:)   -> s "&"
        (:|:)   -> s "|"
        (:~&:)  -> s "~&"
        (:~|:)  -> s "~|"
        (:<~>:) -> s "<~>"

instance (ToTPTP f, ToTPTP t) => ToTPTP (Formula0 t f) where
    toTPTPBuilder formu =
      let
        showParen True x = s "(" <> x <> s ")"
        showParen False x = x

        result =
           case formu of
               Quant q vars f    ->
                   let par = True in

                   toTPTPBuilder q
                      <> s " ["
                      <> commaSepMap toTPTPBuilder vars
                      <> s "] : "
                      <> showParen par (toTPTPBuilder f)

               PredApp p [] -> toTPTPBuilder p
               PredApp p args -> toTPTPBuilder p <> s "(" <> commaSepMap toTPTPBuilder args <> s ")"
               (:~:) f -> s "~ " <> showParen True (toTPTPBuilder f)

               BinOp x op y -> showParen True $
                   (toTPTPBuilder x) <> s " " <> toTPTPBuilder op <> s " " <> (toTPTPBuilder y)

               InfixPred x op y -> showParen True $
                   (toTPTPBuilder x) <> s " " <> toTPTPBuilder op <> s " " <> (toTPTPBuilder y)
      in
        result

instance ToTPTP t => ToTPTP (Term0 t) where
    toTPTPBuilder term =

             case term of
               Var x -> toTPTPBuilder x
               NumberLitTerm d -> showsRational d
               DistinctObjectTerm x -> Builder.stringUtf8 (tptpQuote x)
               FunApp f [] -> toTPTPBuilder f
               FunApp f args -> toTPTPBuilder f <> s "(" <> commaSepMap toTPTPBuilder args <> s ")"


deriving instance (ToTPTP a) => (ToTPTP (Identity a))
deriving instance ToTPTP Formula
deriving instance ToTPTP Term


instance ToTPTP Annotations where
    toTPTPBuilder NoAnnotations = mempty
    toTPTPBuilder (Annotations a b) = s "," <> toTPTPBuilder a <> toTPTPBuilder b

instance ToTPTP UsefulInfo where
    toTPTPBuilder NoUsefulInfo = mempty
    toTPTPBuilder (UsefulInfo xs) = s "," <> s "[" <> commaSepMap toTPTPBuilder xs <> s "]"

instance ToTPTP GTerm where
    toTPTPBuilder gt = case gt of
                  GTerm x -> toTPTPBuilder x
                  ColonSep x y -> toTPTPBuilder x <> s ":" <> toTPTPBuilder y
                  GList xs -> s "[" <> commaSepMap toTPTPBuilder xs <> s "]"

instance ToTPTP AtomicWord where
    toTPTPBuilder (AtomicWord x) = Builder.stringUtf8 $ if isLowerWord x then x else tptpSQuote x

instance ToTPTP GData where
 toTPTPBuilder gd = case gd of
   GWord x -> toTPTPBuilder x
   GApp x args -> toTPTPBuilder x <> s "(" <> commaSepMap toTPTPBuilder args <> s ")"
   GVar x -> toTPTPBuilder x
   GNumber x -> showsRational x
   GDistinctObject x -> Builder.stringUtf8 (tptpQuote x)
   GFormulaData str@"$cnf" formu -> Builder.stringUtf8 str <> s "(" <> cnfToTPTP formu <> s ")"
     where
       cnfToTPTP :: Formula -> Builder.Builder
       cnfToTPTP (F (Identity (BinOp l (:|:) r))) = cnfToTPTP l <> s " | " <> cnfToTPTP r
       cnfToTPTP (F (Identity ((:~:) x@(F (Identity (PredApp _ _)))))) = s "~ " <> toTPTPBuilder x
       cnfToTPTP x@(F (Identity (PredApp _ _))) = toTPTPBuilder x
       -- We do not call toTPTPBuilder directly on the formula in InfixPred case, because parenthesis should not be printed.
       cnfToTPTP (F (Identity (InfixPred x1 (:!=:) x2))) = toTPTPBuilder x1 <> s " != " <> toTPTPBuilder x2
       cnfToTPTP x = error $ show x ++ " is not a literal"

   GFormulaData str formu -> Builder.stringUtf8 str <> s "(" <> toTPTPBuilder formu <> s ")"
   GFormulaTerm str term -> Builder.stringUtf8 str <> s "(" <> toTPTPBuilder term <> s ")"

tptpQuote :: [Char] -> [Char]
tptpQuote x = "\"" ++ concatMap go x ++ "\""
    where
      go '\\' = "\\\\"
      go '"'  = "\\\""
      go c = [c]

tptpSQuote :: [Char] -> [Char]
tptpSQuote x = "'" ++ concatMap go x ++ "'"
    where
      go '\\' = "\\\\"
      go '\''  = "\\'"
      go c = [c]



isBetween :: forall a. (Ord a) => a -> a -> a -> Bool
isBetween a x b = a <= x && x <= b

isReallyAlnum :: Char -> Bool
isReallyAlnum x = isBetween 'a' x 'z' || isBetween 'A' x 'Z' || isBetween '0' x '9' || x=='_'

isLowerWord :: [Char] -> Bool
isLowerWord str = case str of
                               (x:xs) | isBetween 'a' x 'z' && all isReallyAlnum xs -> True
                               _ -> False

instance ToTPTP V where
    toTPTPBuilder (V x) = Builder.stringUtf8 x


showsRational :: Rational -> Builder.Builder
showsRational q = Builder.integerDec (numerator q) <> Builder.char7 '/' <> Builder.integerDec (denominator q)
