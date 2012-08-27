{-# LANGUAGE CPP, TemplateHaskell, NoMonomorphismRestriction, RecordWildCards
  , StandaloneDeriving 
  , TypeSynonymInstances, FlexibleInstances, FlexibleContexts
  , UndecidableInstances, DeriveDataTypeable, GeneralizedNewtypeDeriving
  , OverlappingInstances, ScopedTypeVariables
  #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}

#include "../../MACROS.h"

module Codec.TPTP.Base where
    
import Codec.TPTP.QuickCheck
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Data.Data
import Data.Function
import Data.Monoid hiding(All)
import Data.Set as S hiding(fold)
import Data.String
import Prelude --hiding(concat,foldl,foldl1,foldr,foldr1)
import Test.QuickCheck hiding ((.&.))
import Util
import Data.Pointed
import Data.Copointed
    
-- Should be in the standard library
deriving instance Eq a => Eq (Identity a)
deriving instance Ord a => Ord (Identity a)
deriving instance Show a => Show (Identity a)
deriving instance Read a => Read (Identity a)
deriving instance Data a => Data (Identity a)
deriving instance Typeable1 Identity

-- * Basic undecorated formulae and terms
                   
-- | Basic (undecorated) first-order formulae                   
type Formula = F Identity
    
-- | Basic (undecorated) terms
type Term = T Identity

-- * Formulae and terms decorated with state

-- | First-order formulae decorated with state
type FormulaST s = F (State s)
    
-- | Terms decorated with state
type TermST s = T (State s)

-- | First-order formulae decorated with comments
type FormulaC = FormulaST [String]

-- | Terms decorated with comments
type TermC = TermST [String]

-- | Forget comments in formulae decorated with comments
forgetFC :: FormulaC -> Formula
forgetFC (F f) = F . return $
  case evalState f [] of
    BinOp f1 op f2       -> BinOp (forgetFC f1) op (forgetFC f2)
    InfixPred t1 pred_ t2 -> InfixPred (forgetTC t1) pred_ (forgetTC t2)
    PredApp aw ts        -> PredApp aw (fmap forgetTC ts)
    Quant quant vs f_     -> Quant quant vs (forgetFC f_)
    (:~:) f_              -> (:~:) (forgetFC f_)

-- | Forget comments in terms decorated with comments
forgetTC :: TermC -> Term
forgetTC (T t) = T . return $
  case evalState t [] of
    Var v -> Var v
    NumberLitTerm d -> NumberLitTerm d
    DistinctObjectTerm s -> DistinctObjectTerm s
    FunApp aw ts -> FunApp aw (fmap forgetTC ts)


-- | Equivalence
--
-- Important special case:
--
-- @\(\.\<\=\>\.\) :: 'Formula' -> 'Formula' -> 'Formula'@
(.<=>.) :: POINTED_FORMULA(c) => (F c) -> (F c) -> F c
x .<=>. y = (F . point) $ BinOp  x (:<=>:) y  
  
            
-- | Implication
(.=>.) :: POINTED_FORMULA(c) => (F c) -> (F c) -> F c
x .=>.  y = (F . point) $ BinOp  x (:=>:)  y  
            
-- | Reverse implication
(.<=.) :: POINTED_FORMULA(c) => (F c) -> (F c) -> F c
x .<=.  y = (F . point) $ BinOp  x (:<=:)  y  
            
-- | Disjunction/OR
(.|.) :: POINTED_FORMULA(c) => (F c) -> (F c) -> F c
x .|.   y = (F . point) $ BinOp  x (:|:)   y  
            
-- | Conjunction/AND
(.&.) :: POINTED_FORMULA(c) => (F c) -> (F c) -> F c
x .&.   y = (F . point) $ BinOp  x (:&:)   y  
            
-- | XOR
(.<~>.) :: POINTED_FORMULA(c) => (F c) -> (F c) -> F c
x .<~>. y = (F . point) $ BinOp  x (:<~>:) y  
            
-- | NOR
(.~|.) :: POINTED_FORMULA(c) => (F c) -> (F c) -> F c
x .~|.  y = (F . point) $ BinOp  x (:~|:)  y  
            
            
            
-- | NAND
(.~&.) :: POINTED_FORMULA(c) => (F c) -> (F c) -> F c
x .~&.  y = (F . point) $ BinOp  x (:~&:)  y  
            
            
-- | Negation
(.~.) :: POINTED_FORMULA(c) => (F c) -> F c
(.~.) x = (F . point) $ (:~:) x
          
-- | Equality
(.=.) :: POINTED_FORMULA(c) => (T c) -> (T c) -> F c
x .=. y   = (F . point) $ InfixPred x (:=:)   y 
            
-- | Inequality
(.!=.) :: POINTED_FORMULA(c) => (T c) -> (T c) -> F c
x .!=. y  = (F . point) $ InfixPred x (:!=:) y 
            
-- | Universal quantification
for_all :: POINTED_FORMULA(c) => [V] -> (F c) -> F c
for_all vars x = (F . point) $ Quant All vars x
                 
-- | Existential quantification
exists :: POINTED_FORMULA(c) => [V] -> (F c) -> F c
exists vars x = (F . point) $ Quant Exists vars x
                
-- | Predicate symbol application
pApp :: POINTED_FORMULA(c) => AtomicWord -> [T c] -> F c
pApp x args = (F . point) $ PredApp x args
              
-- | Variable
var :: POINTED_TERM(c) => V -> T c
var = (T . point) . Var
      
-- | Function symbol application (constants are encoded as nullary functions)
fApp :: POINTED_TERM(c) => AtomicWord -> [T c] -> T c
fApp x args = (T . point) $ FunApp x args
              
-- | Number literal
numberLitTerm :: POINTED_TERM(c) => Rational -> T c
numberLitTerm = (T . point) . NumberLitTerm
                
-- | Double-quoted string literal, called /Distinct Object/ in TPTP's grammar 
distinctObjectTerm :: POINTED_TERM(c) => String -> T c
distinctObjectTerm = (T . point) . DistinctObjectTerm
                     
infixl 2  .<=>. ,  .=>. ,  .<=. ,  .<~>.
infixl 3  .|. ,  .~|.
infixl 4  .&. ,  .~&.
infixl 5  .=. ,  .!=.

-- * General decorated formulae and terms
    
-- | See <http://haskell.org/haskellwiki/Indirect_composite> for the point of the type parameters (they allow for future decorations, e.g. monadic subformulae). If you don't need decorations, you can just use 'Formula' and the wrapped constructors above.
data Formula0 term formula = 
              BinOp formula BinOp formula -- ^ Binary connective application
            | InfixPred term InfixPred term -- ^ Infix predicate application (equalities, inequalities)
            | PredApp AtomicWord [term] -- ^ Predicate application
            | Quant Quant [V] formula -- ^ Quantified formula
            | (:~:) formula -- ^ Negation
              deriving (Eq,Ord,Show,Read,Data,Typeable)
                       
                       
-- | See <http://haskell.org/haskellwiki/Indirect_composite> for the point of the type parameters (they allow for future decorations). If you don't need decorations, you can just use 'Term' and the wrapped constructors above.
data Term0 term =
            Var V -- ^ Variable
          | NumberLitTerm Rational -- ^ Number literal
          | DistinctObjectTerm String -- ^ Double-quoted item
          | FunApp AtomicWord [term] -- ^ Function symbol application (constants are encoded as nullary functions) 
            deriving (Eq,Ord,Show,Read,Data,Typeable)
                     

    
    


-- | Binary formula connectives 
data BinOp =
    -- Please don't change the constructor names (the Show instance is significant)
               (:<=>:)  -- ^ Equivalence
            |  (:=>:)  -- ^ Implication
            |  (:<=:)  -- ^ Reverse Implication
            |  (:&:)  -- ^ AND
            |  (:|:)  -- ^ OR
            |  (:~&:)  -- ^ NAND
            |  (:~|:)  -- ^ NOR
            |  (:<~>:)  -- ^ XOR
              deriving (Eq,Ord,Show,Read,Data,Typeable,Enum,Bounded)

-- | /Term -> Term -> Formula/ infix connectives
data InfixPred =
    -- Please don't change the constructor names (the Show instance is significant)
    (:=:) | (:!=:)         
            deriving (Eq,Ord,Show,Read,Data,Typeable,Enum,Bounded)
                       
-- | Quantifier specification
data Quant = All | Exists
              deriving (Eq,Ord,Show,Read,Data,Typeable,Enum,Bounded)
                     
-- * Formula Metadata
    
-- | A line of a TPTP file: Annotated formula, comment or include statement.
type TPTP_Input = TPTP_Input_ Identity
{-
    -- | Annotated formulae
    AFormula {
      name :: AtomicWord 
    , role :: Role 
    , formula :: Formula 
    , annotations :: Annotations 
    }    
    | Comment String
    | Include FilePath [AtomicWord]

    deriving (Eq,Ord,Show,Read,Data,Typeable)
-}

-- | A line of a TPTP file: Annotated formula (with the comment string embbeded in the State monad ), comment or include statement
type TPTP_Input_C = TPTP_Input_ (State [String])

-- | Forget comments in a line of a TPTP file decorated with comments
forgetTIC :: TPTP_Input_C -> TPTP_Input
forgetTIC tic@(AFormula {}) = tic { formula = forgetFC (formula tic) }
forgetTIC (Comment s) = Comment s
forgetTIC (Include p aws) = Include p aws

-- | Generalized TPTP_Input
data TPTP_Input_ c = 
   -- | Annotated formulae
   AFormula {
     name :: AtomicWord 
   , role :: Role 
   , formula :: F c
   , annotations :: Annotations 
   }    
   | Comment String
   | Include FilePath [AtomicWord]

            
deriving instance Eq (c (Formula0 (T c) (F c))) => Eq (TPTP_Input_ c)
deriving instance Ord (c (Formula0 (T c) (F c))) => Ord (TPTP_Input_ c)
deriving instance Show (c (Formula0 (T c) (F c))) => Show (TPTP_Input_ c)
deriving instance Read (c (Formula0 (T c) (F c))) => Read (TPTP_Input_ c)
deriving instance (Typeable1 c, Data (c (Formula0 (T c) (F c)))) => Data (TPTP_Input_ c)
instance Typeable1 c => Typeable (TPTP_Input_ c) where
  typeOf = mkTypeOfForRank2Kind "Codec.TPTP.Base" "TPTP_Input_"

-- | Annotations about the formulas origin                   
data Annotations = NoAnnotations | Annotations GTerm UsefulInfo
                  deriving (Eq,Ord,Show,Read,Data,Typeable)
              
-- | Misc annotations
data UsefulInfo = NoUsefulInfo | UsefulInfo [GTerm]
                  deriving (Eq,Ord,Show,Read,Data,Typeable)
                           
-- | Formula roles
newtype Role = Role { unrole :: String }
            deriving (Eq,Ord,Show,Read,Data,Typeable)


-- | Metadata (the /general_data/ rule in TPTP's grammar)
data GData = GWord AtomicWord
                 | GApp AtomicWord [GTerm]
                 | GVar V
                 | GNumber Rational
                 | GDistinctObject String
                 | GFormulaData String Formula 
                   deriving (Eq,Ord,Show,Read,Data,Typeable)
        
-- | Metadata (the /general_term/ rule in TPTP's grammar)
data GTerm = ColonSep GData GTerm                   
           | GTerm GData
           | GList [GTerm]
             deriving (Eq,Ord,Show,Read,Data,Typeable)
                     

                   

-- * Gathering free Variables

-- class FormulaOrTerm c a where
--     elimFormulaOrTerm :: (F c -> r) -> (T c -> r) -> a -> r

-- instance FormulaOrTerm Identity Formula where
--     elimFormulaOrTerm k _ x = k x
                              
-- instance FormulaOrTerm Identity Term where
--     elimFormulaOrTerm _ k x = k x
                              
class FreeVars a where
    -- | Obtain the free variables from a formula or term
    freeVars :: a -> Set V

-- | Universally quantify all free variables in the formula
univquant_free_vars :: Formula -> Formula
univquant_free_vars cnf = 
    case S.toList (freeVars cnf) of
      [] -> cnf
      vars -> for_all vars cnf
             
instance FreeVars Formula where
    freeVars = foldF 
               freeVars   
               (\_ vars x -> S.difference (freeVars x) (S.fromList vars))                    
               (\x _ y -> (mappend `on` freeVars) x y)
               (\x _ y -> (mappend `on` freeVars) x y)
               (\_ args -> S.unions (fmap freeVars args))

instance FreeVars Term where
    freeVars = foldT
               (const mempty)
               (const mempty)
               S.singleton
               (\_ args -> S.unions (fmap freeVars args))


--- Have the Arbitrary instances in this module to avoid orphan instances

instance Arbitrary TPTP_Input
    where arbitrary = frequency [(10,       
                                    do 
                                      x1 <- AtomicWord <$> arbLowerWord
                                      x2 <- arbitrary
                                      x3 <- arbitrary
                                      x4 <- arbitrary
                                      return (AFormula x1 x2 x3 x4))
                                         
                                  , (1,
                                    do 
                                      x1 <- arbPrintable
                                      return (Comment ("% "++x1))
                                  )

                                  , (1, Include `fmap` arbLowerWord `ap` 
                                         listOf arbitrary)
                                ]

instance Arbitrary Formula
    where arbitrary = fmap (F . point) arbitrary

instance Arbitrary Term
    where arbitrary = fmap (T . point) arbitrary

instance Arbitrary Annotations
    where arbitrary = oneof [
                              return NoAnnotations
                            , Annotations `fmap` arbitrary `ap` arbitrary
                      ]
                      
instance Arbitrary UsefulInfo
    where arbitrary = oneof [
                              return NoUsefulInfo
                             , do 
                                x1 <- arbitrary
                                return (UsefulInfo x1)
                      ]
          
instance Arbitrary Role
    where arbitrary = Role `fmap` arbLowerWord
               
               
instance (Arbitrary a, Arbitrary b) => Arbitrary (Formula0 a b)

    
    where arbitrary = sized go

           where
            go 0 = flip PredApp [] `fmap` arbitrary
                   
            go i =  
                oneof [ do
                                  ileft <- choose (0,i-1)
                                  x1 <- resize ileft arbitrary
                                  x2 <- arbitrary
                                  x3 <- resize (i - 1 - ileft) arbitrary 
                                  return (BinOp x1 x2 x3)
                                         
                      , do 
                                  x1 <- arbitrary
                                  x2 <- arbitrary
                                  x3 <- arbitrary
                                  return (InfixPred x1 x2 x3)
                                         
                      , do
                                  x1 <- arbitrary
                                  x2 <- argsFreq vector
                                  return (PredApp x1 x2)
                                            
                      , do
                               x1 <- arbitrary
                               x2 <- liftM2 (:) arbitrary (argsFreq (\nargs -> vectorOf nargs arbitrary))
                               x3 <- resize (i-1) arbitrary
                               return (Quant x1 x2 x3)
                                           
                      , do
                                  x1 <- resize (i-1) arbitrary
                                  return ((:~:) x1)
                      ]
                                          
instance Arbitrary BinOp
    where arbitrary = elements
                     
                             [ (:<=>:)
                             , (:=>:)
                             , (:<=:)
                             , (:&:)
                             , (:|:)
                             , (:~&:)
                             , (:~|:)
                             , (:<~>:)
                             ]

instance Arbitrary InfixPred
    where arbitrary = elements [ (:=:),(:!=:) ]

instance Arbitrary Quant
    where arbitrary = elements [All,Exists]
                      
instance Arbitrary a => Arbitrary (Term0 a)
    where arbitrary = sized go
           where


            go 0 = frequency [ (2,Var <$> arbitrary), (1,FunApp `fmap` arbitrary `ap` return[] ) ]

            go i = oneof [
                             do 
                              x1 <- arbitrary
                              return (Var x1)
                                     
                           , arbNum NumberLitTerm 
                             
                                     
                           , do 
                              x1 <- arbPrintable
                              return (DistinctObjectTerm x1)
                                     
                           , do 
                              x1 <- arbitrary
                              args <- argsFreq 
                                (\nargs -> do
                                   parti <- arbPartition nargs (i-1)
                                   mapM (flip resize arbitrary) parti
                                )
                                                
                              return (FunApp x1 args)
                           ]

instance Arbitrary GData
    where arbitrary = sized go

                  where
                       go 0 = oneof [ fmap GWord arbitrary
                                    , fmap GVar arbitrary
                                    ]
                    
                       go i = 
                           oneof 
                           [
                            GWord <$> arbitrary
                                            
                           ,do
                              x1 <- arbLowerWord
                              args <- argsFreq 
                                         (\nargs -> do
                                            parti <- arbPartition nargs (i-1)
                                            mapM (flip resize arbitrary) parti
                                         ) `suchThat` ((/=) [])
                                          
                              return (GApp (AtomicWord x1) args)
                                     
                           ,GVar <$> arbitrary
                           ,arbNum GNumber 
                                 
                           ,GDistinctObject <$> arbPrintable
                           ,GFormulaData `fmap` ((:) '$' `fmap` arbLowerWord) `ap` (sized (\n -> resize (n `div` 2) arbitrary))
                           ]

                                 
                                 
instance Arbitrary GTerm
    where arbitrary = sized go

              where
                go 0 = fmap GTerm arbitrary

                go i =
                    oneof [
                            do  
                                  ileft <- choose(0,i-1)
                                  x1 <- resize ileft arbitrary
                                  x2 <- resize (i-1-ileft) arbitrary
                                  return (ColonSep x1 x2)
                                         
                          , do
                                  x1 <- arbitrary
                                  return (GTerm x1)
                                            
                          , do
                                  args <- argsFreq 
                                           (\nargs -> do
                                              parti <- arbPartition nargs (i-1)
                                              mapM (flip resize arbitrary) parti
                                           ) `suchThat` (/= [])
                              
                                  return (GList args)
                       ]
                            
-- | TPTP constant symbol\/predicate symbol\/function symbol identifiers (they are output in single quotes unless they are /lower_word/s). 
-- 
-- Tip: Use the @-XOverloadedStrings@ compiler flag if you don't want to have to type /AtomicWord/ to construct an 'AtomicWord' 
newtype AtomicWord = AtomicWord String
    deriving (Eq,Ord,Show,Data,Typeable,Read,Monoid,IsString)
                                         
instance Arbitrary AtomicWord where
    arbitrary = frequency [  (5, AtomicWord <$> arbLowerWord)
                            ,(1, AtomicWord <$> arbPrintable)
                          ]
                
-- | Variable names
newtype V = V String
    deriving (Eq,Ord,Show,Data,Typeable,Read,Monoid,IsString)
                                         
instance Arbitrary V where
    arbitrary = V <$> arbVar
        
-- * Fixed-point style decorated formulae and terms
             
-- | Formulae whose subexpressions are wrapped in the given type constructor @c@.
--
-- For example:
--
-- - @c = 'Identity'@: Plain formulae
--
-- - @c = 'Maybe'@: Formulae that may contain \"holes\"
--
-- - @c = 'IORef'@: (Mutable) formulae with mutable subexpressions 
newtype F c = F { runF :: c (Formula0 (T c) (F c)) }
    
-- | Terms whose subterms are wrapped in the given type constructor @c@
newtype T c = T { runT :: c (Term0 (T c)) }
             
#define DI(X) deriving instance (X (c (Term0 (T c)))) => X (T c); deriving instance (X (c (Formula0 (T c) (F c)))) => X (F c)
    
DI(Eq)
DI(Ord)
DI(Show)
DI(Read)
  
instance Typeable1 c => Typeable (F c) where
    typeOf = mkTypeOfForRank2Kind "Codec.TPTP.Base" "F"
    
instance Typeable1 c => Typeable (T c) where
    typeOf = mkTypeOfForRank2Kind "Codec.TPTP.Base" "T"

deriving instance (Typeable1 c, Data (c (Term0 (T c))))  => Data (T c)
deriving instance (Typeable1 c, Data (c (Formula0 (T c) (F c)))) => Data (F c)
  
-- * Utility functions

unwrapF ::
            (Copointed t) =>
            F t -> Formula0 (T t) (F t)
unwrapF (F x) = copoint x
unwrapT ::
            (Copointed t) =>
            T t -> Term0 (T t)
unwrapT (T x) = copoint x

foldFormula0 ::
                  (f -> r)
                -> (Quant -> [V] -> f -> r)
                -> (f -> BinOp -> f -> r)
                -> (t -> InfixPred -> t -> r)
                -> (AtomicWord -> [t] -> r)
                -> Formula0 t f
                -> r
foldFormula0 kneg kquant kbinop kinfix kpredapp f =
    case f of
      (:~:) x -> kneg x
      Quant x y z -> kquant x y z
      BinOp x y z -> kbinop x y z
      InfixPred x y z -> kinfix x y z
      PredApp x y -> kpredapp x y
                      
foldTerm0 ::
               (String -> r)
             -> (Rational -> r)
             -> (V -> r)
             -> (AtomicWord -> [t] -> r)
             -> Term0 t
             -> r
foldTerm0 kdistinct knum kvar kfunapp t =
    case t of
      DistinctObjectTerm x -> kdistinct x
      NumberLitTerm x -> knum x
      Var x -> kvar x
      FunApp x y -> kfunapp x y


-- | Eliminate formulae
foldF ::
         (Copointed t) =>
           (F t -> r) -- ^ Handle negation
         -> (Quant -> [V] -> F t -> r) -- ^ Handle quantification
         -> (F t -> BinOp -> F t -> r) -- ^ Handle binary op
         -> (T t -> InfixPred -> T t -> r) -- ^ Handle equality/inequality
         -> (AtomicWord -> [T t] -> r) -- ^ Handle predicate symbol application
         -> (F t -> r) -- ^ Handle formula
         
foldF kneg kquant kbinop kinfix kpredapp f = foldFormula0 kneg kquant kbinop kinfix kpredapp (unwrapF f)

-- | Eliminate terms
foldT ::
         (Copointed t) =>
           (String -> r) -- ^ Handle string literal
         -> (Rational -> r) -- ^ Handle number literal
         -> (V -> r) -- ^ Handle variable
         -> (AtomicWord -> [T t] -> r) -- ^ Handle function symbol application
         -> (T t -> r) -- ^ Handle term
foldT kdistinct knum kvar kfunapp t = foldTerm0 kdistinct knum kvar kfunapp (unwrapT t)
