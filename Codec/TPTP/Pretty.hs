{-# OPTIONS -XRecordWildCards -XCPP -XDeriveDataTypeable -XNoMonomorphismRestriction -XStandaloneDeriving -XFlexibleInstances -XFlexibleContexts -XGeneralizedNewtypeDeriving #-}

module Codec.TPTP.Pretty where

import Codec.TPTP.Base
import Text.PrettyPrint.ANSI.Leijen

oper = red . text
prettyvar = blue . text
psym = green . text
fsym = yellow . text
arguments = fillSep . punctuate comma . fmap pretty
            
data WithPrec a = Prec !String a

needsParen inner outer =
    case (inner,outer) of
      -- special handling for associative ops
      ("∧","∧") -> False
      ("∨","∨") -> False
      ("XOR","XOR") -> False
      
      -- chains of quantifiers/negation don't need parentheses
      otherwise 
          | let u = ["∀","∃","¬"] 
            in inner `elem` u && outer `elem` u 

                -> False
                  
      otherwise -> prec inner <= prec outer
  where
    prec x = case x of 
               "=" -> 60
               "≠" -> 60
               "∀" -> 50
               "∃" -> 50
               "¬" -> 50
               "∧" -> 40
               "NAND" -> 40
               "∨" -> 30
               "NOR" -> 30
               "⇔" -> 20
               "⇐" -> 20
               "⇒" -> 20
               "" -> (-1000) -- if there's no enclosing operation, we don't need parentheses


                   
instance Pretty TPTP_Input where
    pretty AFormula{..} 
        = (red.text) "AFormula"
          </>
          vsep
          [
            text name
          ,(underline.text.unrole) role
          , pretty formula
          ,(text.show) sourceInfo
          ,(text.show) usefulInfo
          ]
                           
    pretty (Comment str)
        = magenta . string $ str
                           
    prettyList = foldr (\x xs -> pretty x <$> empty <$> xs) empty 
               
                       
                       
instance (Pretty (WithPrec t), Pretty (WithPrec f)) => Pretty ((WithPrec (Formula0 t f))) where
    pretty (Prec enclosing formu) = 
     let 
         quant str vars f = 
             (if needsParen str enclosing then parens else id)
             (oper str <+> brackets (hsep (fmap prettyvar vars)) <> dot </> pretty (Prec str f))
                          
         binop f1 str f2 = 
             (if needsParen str enclosing then parens else id)
             (pretty (Prec str f1) </> oper str </> pretty (Prec str f2))
                           

     in
             case formu of
               All vars f    -> quant "∀" vars f
               Exists vars f -> quant "∃" vars f
               PApp p args -> psym p </> parens (arguments (fmap (Prec "") args))
               FromTerm t -> text "fromTerm" </> parens (pretty (Prec "" t))
               (:~:) f -> (if needsParen "¬" enclosing then parens else id) (oper "¬" </> pretty (Prec "¬" f))
          
               f1 :<=>: f2 -> binop f1 "⇔" f2
               f1 :=>: f2 -> binop f1 "⇒" f2
               f1 :<=: f2 -> binop f1 "⇐" f2
               f1 :&: f2 -> binop f1 "∧" f2
               f1 :|: f2 -> binop f1 "∨" f2
               f1 :~&: f2 -> binop f1 "NAND" f2
               f1 :~|: f2 -> binop f1 "NOR" f2
               f1 :<~>: f2 -> binop f1 "XOR" f2
               t1 :=: t2 -> binop t1 "=" t2
               t1 :!=: t2 -> binop t1 "≠" t2
                                
instance (Pretty (WithPrec t)) => Pretty (WithPrec (Term0 t)) where
    pretty (Prec enclosing x) = 
        case x of 
          Var s -> prettyvar s
          NumberLitTerm d -> text (show d)
          DistinctObjectTerm s -> text s
          FApp f args -> fsym f <> parens(arguments (fmap (Prec "") args))
                        
                        

instance Pretty (Formula0 Term Formula) where
    pretty = pretty . Prec "" . FF
                   
instance Pretty (Term0 Term) where
    pretty = pretty . Prec "" . TT
             
instance Pretty (WithPrec Formula) where
    pretty (Prec x (FF y)) = pretty (Prec x y) 
                             
instance Pretty (WithPrec Term) where
    pretty (Prec x (TT y)) = pretty (Prec x y) 
                             
deriving instance Pretty Formula
deriving instance Pretty Term
                     
-- instance (Pretty f, Pretty t) => Pretty (Formula0 t f) where
--     pretty = pretty . Prec ""
                   
-- instance (Pretty t) => Pretty (Term0 t) where
--     pretty = pretty . Prec ""
                       
prettySimple :: Pretty a => a -> String
prettySimple x = displayS (renderPretty 0.9 80 (pretty x)) ""
