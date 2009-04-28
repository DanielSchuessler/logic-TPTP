{-# OPTIONS -fglasgow-exts -XStandaloneDeriving -XRecordWildCards -XNoMonomorphismRestriction -fwarn-missing-signatures #-}

module Codec.TPTP.Export(toTPTP',ToTPTP(..),isLowerWord) where
    
import Codec.TPTP.Base
    
-- | Convenient wrapper for 'toTPTP'
toTPTP' :: forall a. (ToTPTP a) => a -> String
toTPTP' = ($"") . toTPTP
    
s :: String -> String -> String
s = showString
comma :: String -> String
comma = s ","
commaSepMap :: forall a.
               (a -> String -> String) -> [a] -> String -> String
commaSepMap f [] = s ""
commaSepMap f (y:ys) = f y . foldr (\x xs -> comma . f x . xs) id ys 
                       

        
    
class ToTPTP a where
    -- | Convert to TPTP
    toTPTP :: a -> ShowS
             
instance ToTPTP [TPTP_Input] where
    toTPTP = foldr (\x xs -> toTPTP x . s "\n" . xs) id

instance ToTPTP TPTP_Input where
    toTPTP AFormula{..} =
        s "fof(" . toTPTP name . comma . toTPTP role . comma .
          toTPTP formula . toTPTP sourceInfo . toTPTP usefulInfo . s ")."
                 
    toTPTP (Comment x) =
        s x -- % included in x 
                           
    toTPTP (Include x sel) = s "include" . s "(" . showString (tptpSQuote x) . 

                             case sel of { [] -> id; _ -> s ",[" . commaSepMap toTPTP sel . s "]" } . 


                             s ")."  
                   

instance ToTPTP Role where
    toTPTP (Role x) = s x
                      
instance ToTPTP Quant where
    toTPTP All = s "!"
    toTPTP Exists = s "?"
                    
instance ToTPTP InfixPred where
    toTPTP = s . reverse . drop 1 . reverse . drop 1 . show -- {-# EVIL #-} 
             
instance ToTPTP BinOp where
    toTPTP = s . reverse . drop 1 . reverse . drop 1 . show -- {-# EVIL #-}

instance (ToTPTP f, ToTPTP t) => ToTPTP (Formula0 t f) where
    toTPTP formu = 
      let
        result =
           case formu of
               Quant q vars f    -> 
                   let par = True in

                   toTPTP q 
                      . s " [" 
                      . commaSepMap s vars 
                      . s "] : " 
                      . showParen par (toTPTP f)
                          
               PredApp p [] -> toTPTP p
               PredApp p args -> toTPTP p . s "(" . commaSepMap toTPTP args . s ")" 
               (:~:) f -> s "~ " . showParen True (toTPTP f)
                         
               BinOp x op y -> showParen True $ 
                   (toTPTP x) . s " " . toTPTP op . s " " . (toTPTP y)
                   
               InfixPred x op y -> showParen True $ 
                   (toTPTP x) . s " " . toTPTP op . s " " . (toTPTP y)
      in
        result
        
instance ToTPTP t => ToTPTP (Term0 t) where
    toTPTP term =
         
             case term of 
               Var x -> s x
               NumberLitTerm d -> shows d
               DistinctObjectTerm x -> showString (tptpQuote x)
               FunApp f [] -> toTPTP f
               FunApp f args -> toTPTP f . s "(" . commaSepMap toTPTP args . s ")"
                        

         
deriving instance ToTPTP Formula
deriving instance ToTPTP Term


instance ToTPTP SourceInfo where
    toTPTP NoSourceInfo = s ""
    toTPTP (SourceInfo x) = s "," . toTPTP x

instance ToTPTP UsefulInfo where
    toTPTP NoUsefulInfo = s ""
    toTPTP (UsefulInfo xs) = s "," . s "[" . commaSepMap toTPTP xs . s "]"

instance ToTPTP GTerm where
    toTPTP gt = case gt of
                  GTerm x -> toTPTP x
                  ColonSep x y -> toTPTP x . s ":" . toTPTP y
                  GList xs -> s "[" . commaSepMap toTPTP xs . s "]"
                             
instance ToTPTP AtomicWord where
    toTPTP (AtomicWord x) = s $ if isLowerWord x then x else tptpSQuote x      
          
instance ToTPTP GData where
 toTPTP gd = case gd of
   GWord x -> toTPTP x
   GApp x args -> toTPTP x . s "(" . commaSepMap toTPTP args . s ")"
   GVar x -> s x
   GNumber x -> shows x
   GDistinctObject x -> showString (tptpQuote x)
   GFormulaData str formu -> s str . s "(" . toTPTP formu . s ")" 
 
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
isLowerWord x = case x of
                               (x:xs) | isBetween 'a' x 'z' && all isReallyAlnum xs -> True
                               _ -> False
