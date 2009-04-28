{-# OPTIONS -fwarn-missing-signatures -XRecordWildCards -XCPP -XDeriveDataTypeable -fglasgow-exts -XNoMonomorphismRestriction -XTemplateHaskell -XUndecidableInstances -Wall -XOverloadedStrings #-}

module Codec.TPTP.Diff(Diffable(..),DiffResult(..),T0Diff,F0Diff,isSame,diffGenF,diffGenT,printSampleDiffs) where
    
    
-- Warning: This module is a MESS (but it seems to work :)).
    
import Data.Generics
import Control.Applicative()
--import Data.Foldable
import Prelude hiding (putStrLn)
--import Data.Foldable 
--import Test.QuickCheck.Instances
import Test.QuickCheck hiding((.&.))
import Data.Char()
import Control.Monad
import Debug.Trace()
import Codec.TPTP.Base
import Codec.TPTP.Pretty
import Text.PrettyPrint.ANSI.Leijen
import System.UTF8IO
    
    
    

data DiffResult d = 
          Same -- ^ Both arguments are the same. 
        | SameHead d -- ^ The arguments are recursive expressions of the same form, but their subterms differ. Return an "decorated" term that shows where the differences are 
        | Differ d d -- ^ The arguments differ and are not of similar form (don't recurse)
        | DontCare
    deriving (Eq,Ord,Show,Data,Typeable,Read)
                  
isSame :: forall t. DiffResult t -> Bool
isSame Same = True
isSame _ = False
             
-- | Abstraction for diff'ing [predicate symbol|function symbol|gdata] applications
-- Suggestive variable names for the case of function symbol applications
handleAppExpr :: (Eq str) => 
                (term -> term -> termfix DiffResult) -- ^ the diff function 
                
              -> (DiffResult a -> termfix DiffResult) -- ^ newtype wrapper 
              -> (termfix DiffResult -> DiffResult a) -- ^ newtype unwrapper
                
              -> (str -> [termfix DiffResult] -> b) -- ^ Application expression constructor
                
              -> str -- ^ First funsym
              -> [term] -- ^ First args
                
              -> str -- ^ Second funsum
              -> [term] -- ^ Second args
                
              -> DiffResult b
handleAppExpr recFun newtypewrapper newtypeunwrapper constr x1 args1 x2 args2 =
                 -- recurse if heads and arities agree
                 case (x1==x2 && length args1 == length args2) of
                   True -> 
                       let
                           rec = zipWith recFun args1 args2
                       in
                         case all (isSame . newtypeunwrapper) rec of
                           True -> Same
                           False -> SameHead (constr x1 rec) 
                                  
                   False -> 
                       Differ (constr x1 (fmap (const . newtypewrapper $ DontCare) args1))
                              (constr x2 (fmap (const . newtypewrapper $ DontCare) args2))
                              
handleBinExpr :: 
                 (Eq op) =>
                 (term -> term -> termfix DiffResult)
                 
                 -> (DiffResult a -> termfix DiffResult) -- ^ NT wrapper
                 -> (termfix DiffResult -> DiffResult a) -- ^ NT unwrapepr
                   
                 -> (termfix DiffResult -> op -> termfix DiffResult -> b) -- ^ constructor
                   
                 -> term
                 -> op
                 -> term
                   
                 -> term
                 -> op
                 -> term
                   
                 -> DiffResult b
handleBinExpr recFun newtypewrapper newtypeunwrapper constr l1 op1 r1 l2 op2 r2 =
    case op1==op2 of
      True -> 
          let 
              recl = recFun l1 l2
              recr = recFun r1 r2
          in
            case (newtypeunwrapper recl,newtypeunwrapper recr) of
              (Same,Same) -> Same
              (_,_)       -> SameHead (constr recl op1 recr)
                            
      False ->
          let dc = newtypewrapper DontCare 
                   
          in Differ (constr dc op1 dc)
                    (constr dc op2 dc)
                         
handleUnary :: 
                 (term -> term -> termfix DiffResult)
                 
                 -> (DiffResult a -> termfix DiffResult) -- ^ NT wrapper
                 -> (termfix DiffResult -> DiffResult a) -- ^ NT unwrapepr
                   
                 -> (termfix DiffResult -> b) -- ^ constructor
                   
                 -> term
                   
                 -> term
                   
                 -> DiffResult b
                   
handleUnary recFun _ newtypeunwrapper constr r1 r2 =
          let 
              rec = recFun r1 r2
          in
            case newtypeunwrapper rec of
              Same -> Same
              _    -> SameHead (constr rec)
                            

                 
handleLeaf :: 
              (Eq a) =>
              (a -> d) -> a -> a -> DiffResult d
handleLeaf constr x1 x2 = 
    if x1==x2 
    then Same 
    else Differ (constr x1) (constr x2)
                   
                   
             
-- runDiffTerm :: Term
--                -> Term
--                -> DiffResult (Term0 (TermFix DiffResult))
-- runDiffTerm t1 t2 = runTermFix (diffTerm t1 t2)

diffTerm :: Term -> Term -> TermFix DiffResult
diffTerm (TT t1) (TT t2) = TermFix $
    case (t1,t2) of
      (FunApp x1 args1,FunApp x2 args2) -> handleAppExpr diffTerm TermFix runTermFix FunApp x1 args1 x2 args2 
      (Var x1,Var x2) -> handleLeaf Var x1 x2
      (DistinctObjectTerm x1, DistinctObjectTerm x2) -> handleLeaf DistinctObjectTerm x1 x2
      (NumberLitTerm x1, NumberLitTerm x2) -> handleLeaf NumberLitTerm x1 x2
      _ -> let plug=plugSubterms (TermFix DontCare) in Differ (plug t1) (plug t2)
          
plugSubterms :: forall a a1. a -> Term0 a1 -> Term0 a
plugSubterms p t = case t of
                            FunApp x1 args -> FunApp x1 (fmap (const p) args)
                            DistinctObjectTerm x1 -> DistinctObjectTerm x1
                            NumberLitTerm x1 -> NumberLitTerm x1
                            Var x1 -> Var x1
                                     
plugSubformulae :: forall a formula a1 t.
                   a -> formula -> Formula0 a1 t -> Formula0 a formula
plugSubformulae pt pf f = case f of
                               Quant q vars _ -> Quant q vars pf
                               PredApp x1 args -> PredApp x1 (fmap (const pt) args)
                               BinOp _ b _ -> BinOp pf b pf
                               InfixPred _ b _ -> InfixPred pt b pt
                               (:~:) _ -> (:~:) pf
                                                                  
                                   
          
diffFormula :: Formula -> Formula -> FormulaFix DiffResult
diffFormula (FF f1) (FF f2) = FormulaFix $
    case (f1,f2) of
      ( Quant q1 vars1 g1
       ,Quant q2 vars2 g2) -> 
          case (q1==q2, vars1==vars2) of
            (True,True) -> handleUnary diffFormula FormulaFix runFormulaFix (Quant q1 vars1) g1 g2   
            _ -> Differ (Quant q1 vars1 (FormulaFix DontCare)) 
                       (Quant q2 vars2 (FormulaFix DontCare)) 
                           

      ( BinOp l1 op1 r1
       ,BinOp l2 op2 r2 ) -> handleBinExpr diffFormula FormulaFix runFormulaFix BinOp l1 op1 r1 l2 op2 r2
                                         
      ( InfixPred l1 op1 r1
       ,InfixPred l2 op2 r2 ) -> handleBinExpr diffTerm TermFix runTermFix InfixPred l1 op1 r1 l2 op2 r2
                 
      ( (:~:) g1
       ,(:~:) g2 ) -> handleUnary diffFormula FormulaFix runFormulaFix (:~:) g1 g2

      
      ( PredApp x1 args1
       ,PredApp x2 args2 ) -> handleAppExpr diffTerm TermFix runTermFix PredApp x1 args1 x2 args2 
                       
      _ -> let plug=plugSubformulae (TermFix DontCare) (FormulaFix DontCare) in Differ (plug f1) (plug f2)


instance Show (TermFix DiffResult) where
    show (TermFix t) = show t

instance Show (FormulaFix DiffResult) where
    show (FormulaFix f) = show f
         
type T0Diff = DiffResult (Term0 (TermFix DiffResult))
type F0Diff = DiffResult (Formula0 (TermFix DiffResult) (FormulaFix DiffResult))
    

wildcard :: AtomicWord
wildcard = "_"
                            
instance Pretty (WithEnclosing T0Diff) where
    pretty = prettyHelper (plugSubterms (TT (FunApp wildcard [])))
 
instance Pretty (WithEnclosing F0Diff) where
    pretty = prettyHelper (plugSubformulae (TT (FunApp wildcard [])) (FF (PredApp wildcard [])))
       
prettyHelper :: forall t a.
                (Pretty (WithEnclosing t), Pretty (WithEnclosing a)) =>
                (t -> a) -> WithEnclosing (DiffResult t) -> Doc
prettyHelper _ (WithEnclosing _ d) = 
        let 
            pwe = pretty . WithEnclosing EnclNothing
        in
          case d of
                 DontCare -> dullwhite.pretty $ wildcard
                 Same -> dullgreen.text $ "Same"
                 SameHead x -> blue . encloseSep (text "{ SameHead ") (text "}") semi $
                              [  pwe x
                              ]
                                               

                              
                 Differ x y -> red . encloseSep (text "{ Differ ") rbrace (text "; ") $

                              [ pwe x 
                              , pwe y
                              ]
                                                         
deriving instance Pretty (TermFix DiffResult)

deriving instance Pretty (FormulaFix DiffResult)
         
instance Pretty (WithEnclosing (TermFix DiffResult)) where
    pretty (WithEnclosing x (TermFix y)) = pretty (WithEnclosing x y) 
                                           
instance Pretty (WithEnclosing (FormulaFix DiffResult)) where
    pretty (WithEnclosing x (FormulaFix y)) = pretty (WithEnclosing x y) 
                                              
instance Pretty (T0Diff) where
    pretty = pretty . WithEnclosing EnclNothing
    
instance Pretty (F0Diff) where
    pretty = pretty . WithEnclosing EnclNothing

-- instance Pretty (DiffResult (TermFix DiffResult)) where
--     pretty = pretty . WithEnclosing EnclNothing . TermFix
    
-- instance Pretty (DiffResult (TermFix DiffResult) (FormulaFix DiffResult)) where
--     pretty = pretty . WithEnclosing EnclNothing . FormulaFix
                                              
                                  
-- runFormulaDiff :: Formula
--                   -> Formula
--                   -> DiffResult
--                        (Formula0 (TermFix DiffResult) (FormulaFix DiffResult))
-- runFormulaDiff a b = runFormulaFix (diffFormula a b)
                     
-- | Less random generator for generating formulae suitable for testing diff
diffGenF :: Gen Formula
diffGenF = sized go
            where
              go 0 = return $ pApp "Truth" []
              go i = oneof 
                  [
                   do
                     ileft <- choose (0,i-1)
                     liftM2 (.&.) (resize ileft diffGenF) (resize (i-1-ileft) diffGenF)
                  , let a=diffGenT in fmap (.=.) a `ap` a
                  ]
           
diffGenT :: Gen Term
diffGenT = sized go
            where
              go 0 = elements[var "Z",fApp "1" []]
                  
              go i = oneof 
                  [
                    let a=resize (i`div`3) diffGenT in fmap (fApp "g") (sequence [a,a,a])
                  , do
                     ileft <- choose (0,i-1)
                     fmap (fApp "f") (sequence [resize ileft diffGenT,
                                                resize (i-1-ileft) diffGenT])
                  ]
             
                     
printSampleDiffs :: IO ()
printSampleDiffs = do
  xs <- sample' diffGenF
  ys <- sample' diffGenF

  let item x y = text (replicate 50 '=') 
                 <$> pretty x
                 <$> text (replicate 40 '-')
                 <$> pretty y
                 <$> text (replicate 40 '-')
                 <$> pretty (diffFormula x y)
  
  mapM_ (putStrLn . prettySimple . uncurry item) (zip xs ys)

-- prop0 :: Formula -> Formula -> Property
-- prop0 f1 f2 = f1 /= f2 ==>

--               let res=diffFormula (pApp "foo" [] .&. f1)
--                                   (pApp "foo" [] .&. f2)

--               in collect res $ True
                 
--                  -- case res of
--                  --   SameHead ((FormulaFix Same) :&: 
--                  --            (FormulaFix (Differ f1  

instance Functor DiffResult where
    fmap f d = case d of
                 Same -> Same
                 SameHead x -> SameHead (f x)
                 Differ x y -> Differ (f x) (f y)
                 DontCare -> DontCare




class Diffable a b where
    diff :: a -> a -> b

instance Diffable Formula (FormulaFix DiffResult) where diff = diffFormula
instance Diffable Term (TermFix DiffResult) where diff = diffTerm
