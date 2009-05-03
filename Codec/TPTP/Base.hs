{-# OPTIONS -XRecordWildCards -XCPP 
 -XDeriveDataTypeable -fglasgow-exts -XNoMonomorphismRestriction 
 -XTemplateHaskell -XGeneralizedNewtypeDeriving -Wall 
 -fno-warn-orphans -XOverlappingInstances -XUndecidableInstances 
 #-}

module Codec.TPTP.Base where
    
import Data.Generics
import Data.Set as S hiding(fold)
import Control.Applicative
--import Data.Foldable
import Prelude --hiding(concat,foldl,foldl1,foldr,foldr1)
--import Data.Foldable 
--import Test.QuickCheck.Instances
import Test.QuickCheck hiding ((.&.))
import Data.Char
import Codec.TPTP.QuickCheck
import Data.String
import Data.Monoid hiding(All)
import Control.Monad.Identity
import Data.Function
    
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
    
    
(.<=>.) :: 
           (Pointed (Formula0 (T c) (F c)) (c (Formula0 (T c) (F c)))) =>
           (F c) -> (F c) -> F c
x .<=>. y = (F . point) $ BinOp  x (:<=>:) y  
            
(.<~>.) :: 
           (Pointed (Formula0 (T c) (F c)) (c (Formula0 (T c) (F c)))) =>
           (F c) -> (F c) -> F c
x .<~>. y = (F . point) $ BinOp  x (:<~>:) y  
            
(.=>.) :: 
          (Pointed (Formula0 (T c) (F c)) (c (Formula0 (T c) (F c)))) =>
          (F c) -> (F c) -> F c
x .=>.  y = (F . point) $ BinOp  x (:=>:)  y  
            
(.<=.) :: 
          (Pointed (Formula0 (T c) (F c)) (c (Formula0 (T c) (F c)))) =>
          (F c) -> (F c) -> F c
x .<=.  y = (F . point) $ BinOp  x (:<=:)  y  
            
(.~|.) :: 
          (Pointed (Formula0 (T c) (F c)) (c (Formula0 (T c) (F c)))) =>
          (F c) -> (F c) -> F c
x .~|.  y = (F . point) $ BinOp  x (:~|:)  y  
            
(.|.) :: 
         (Pointed (Formula0 (T c) (F c)) (c (Formula0 (T c) (F c)))) =>
         (F c) -> (F c) -> F c
x .|.   y = (F . point) $ BinOp  x (:|:)   y  
            
(.~&.) :: 
          (Pointed (Formula0 (T c) (F c)) (c (Formula0 (T c) (F c)))) =>
          (F c) -> (F c) -> F c
x .~&.  y = (F . point) $ BinOp  x (:~&:)  y  
            
(.&.) :: 
         (Pointed (Formula0 (T c) (F c)) (c (Formula0 (T c) (F c)))) =>
         (F c) -> (F c) -> F c
x .&.   y = (F . point) $ BinOp  x (:&:)   y  
            
(.~.) :: 
         (Pointed (Formula0 (T c) (F c)) (c (Formula0 (T c) (F c)))) =>
         (F c) -> F c
(.~.) x = (F . point) $ (:~:) x
          
          
(.=.) :: 
         (Pointed (Formula0 (T c) (F c)) (c (Formula0 (T c) (F c)))) =>
         (T c) -> (T c) -> F c
x .=. y   = (F . point) $ InfixPred x (:=:)   y 
            
(.!=.) :: 
          (Pointed (Formula0 (T c) (F c)) (c (Formula0 (T c) (F c)))) =>
          (T c) -> (T c) -> F c
x .!=. y  = (F . point) $ InfixPred x (:!=:) y 
            
for_all :: 
           (Pointed (Formula0 (T c) (F c)) (c (Formula0 (T c) (F c)))) =>
           [V] -> (F c) -> F c
for_all vars x = (F . point) $ Quant All vars x
                 
exists :: 
          (Pointed (Formula0 (T c) (F c)) (c (Formula0 (T c) (F c)))) =>
          [V] -> (F c) -> F c
exists vars x = (F . point) $ Quant Exists vars x
                
pApp :: 
        (Pointed (Formula0 (T c) (F c)) (c (Formula0 (T c) (F c)))) =>
        AtomicWord -> [T c] -> F c
pApp x args = (F . point) $ PredApp x args
              
var :: 
       (Pointed (Term0 (T c)) (c (Term0 (T c)))) =>
       V -> T c
var = (T . point) . Var
      
fApp :: 
        (Pointed (Term0 (T c)) (c (Term0 (T c)))) =>
        AtomicWord -> [T c] -> T c
fApp x args = (T . point) $ FunApp x args
              
numberLitTerm :: 
                 (Pointed (Term0 (T c)) (c (Term0 (T c)))) =>
                 Double -> T c
numberLitTerm = (T . point) . NumberLitTerm
                
distinctObjectTerm :: 
                      (Pointed (Term0 (T c)) (c (Term0 (T c)))) =>
                      String -> T c
distinctObjectTerm = (T . point) . DistinctObjectTerm
                     
infixl 2  .<=>. ,  .=>. ,  .<=. ,  .<~>.
infixl 3  .|. ,  .~|.
infixl 4  .&. ,  .~&.
infixl 5  .=. ,  .!=.

-- * General decorated formulae and terms
    
-- | See <http://haskell.org/haskellwiki/Indirect_composite> for the point of the type parameters (they allow for future decorations). If you don't need decorations, you can just use 'Formula' and the wrapped constructors above.
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
          | NumberLitTerm Double -- ^ Number literal
          | DistinctObjectTerm String -- ^ Double-quoted item
          | FunApp AtomicWord [term] -- ^ Function symbol application (constants are nullary functions) 
            deriving (Eq,Ord,Show,Read,Data,Typeable)
                     
-- | Binary formula connectives 
data BinOp =
    -- Please don't change the constructor names (the Show instance is significant)
               (:<=>:)  -- ^ Equivalence
            |  (:=>:)  -- ^ Implication
            |  (:<=:)  -- ^ Implication (reverse)
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
data TPTP_Input = 
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
             
             
            

-- | Annotations about the formulas origin                   
data Annotations = NoAnnotations | Annotations GTerm UsefulInfo
                  deriving (Eq,Ord,Show,Read,Data,Typeable)
              
-- | Misc annotations
data UsefulInfo = NoUsefulInfo | UsefulInfo [GTerm]
                  deriving (Eq,Ord,Show,Read,Data,Typeable)
                           
-- | Formula roles
data Role = Role { unrole :: String }
            deriving (Eq,Ord,Show,Read,Data,Typeable)


-- | Metadata (the /general_data/ rule in TPTP's grammar)
data GData = GWord AtomicWord
                 | GApp AtomicWord [GTerm]
                 | GVar V
                 | GNumber Double
                 | GDistinctObject String
                 | GFormulaData String Formula 
                   deriving (Eq,Ord,Show,Read,Data,Typeable)
        
-- | Metadata (the /general_term/ rule in TPTP's grammar)
data GTerm = ColonSep GData GTerm                   
           | GTerm GData
           | GList [GTerm]
             deriving (Eq,Ord,Show,Read,Data,Typeable)
                     

                   

-- * Gathering free Variables

class FormulaOrTerm c a where
    elimFormulaOrTerm :: (F c -> r) -> (T c -> r) -> a -> r

instance FormulaOrTerm Identity Formula where
    elimFormulaOrTerm k _ x = k x
                              
instance FormulaOrTerm Identity Term where
    elimFormulaOrTerm _ k x = k x
                              
class FreeVars a where
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
               
#define TRACE(X) id
               
instance (Arbitrary a, Arbitrary b) => Arbitrary (Formula0 a b)

    
    where arbitrary = sized (\n -> TRACE("arbitrary/Formula0") go n)

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
    where arbitrary = sized (\n -> TRACE("arbitrary/Term0") go n)
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
                            
-- | TPTP constant symbol/predicate symbol/function symbol identifiers (they are output in single quotes unless they are /lower_word/s). 
-- 
-- Tip: Use the @-XOverloadedStrings@ compiler flag if you don't want to type /AtomicWord/ to construct an 'AtomicWord' 
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

-- | For a given type constructor @c@, make the fixed point type @Y@ satisfying: 
--
-- > Y = c (Term0 Y)
--
-- (modulo newtype wrapping). See for example 'diffFormula'.
newtype T c = T { runT :: c (Term0 (T c)) }
    
-- | For a given type constructor @c@, make the fixed point type @X@ satisfying: 
--
-- > X = c (Formula0 Y X) 
-- > Y = c (Term0 Y)
--
-- (modulo newtype wrapping). See for example 'diffTerm'.
newtype F c = F { runF :: c (Formula0 (T c) (F c)) }
             
             
#define DI(X) deriving instance (X (c (Term0 (T c)))) => X (T c); deriving instance (X (c (Formula0 (T c) (F c)))) => X (F c)
    
DI(Eq)
DI(Ord)
DI(Show)
DI(Read)
  
instance Typeable1 c => Typeable (F c) where
    typeOf =
        let tc = mkTyCon "F"
        in (\(F x) -> mkTyConApp tc [typeOf1 x]) 
    
instance Typeable1 c => Typeable (T c) where
    typeOf =
        let tc = mkTyCon "T"
        in (\(T x) -> mkTyConApp tc [typeOf1 x]) 

deriving instance (Typeable1 c, Data (c (Term0 (T c))))  => Data (T c)
deriving instance (Typeable1 c, Data (c (Formula0 (T c) (F c)))) => Data (F c)
  
        
class Pointed a b | b -> a where
    point :: a -> b

instance (Monad m) => Pointed a (m a) where
    point = return
            
class Copointed a b | b -> a where
    copoint :: b -> a

instance Copointed a (Identity a) where
    copoint (Identity x) = x

                           
unwrapF ::
            (Copointed (Formula0 (T t) (F t)) (t (Formula0 (T t) (F t)))) =>
            F t -> Formula0 (T t) (F t)
unwrapF (F x) = copoint x
unwrapT ::
            (Copointed (Term0 (T t)) (t (Term0 (T t)))) =>
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
             -> (Double -> r)
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


                   
foldF ::
         (Copointed (Formula0 (T t) (F t)) (t (Formula0 (T t) (F t)))) =>
           (F t -> r)
         -> (Quant -> [V] -> F t -> r)
         -> (F t -> BinOp -> F t -> r)
         -> (T t -> InfixPred -> T t -> r)
         -> (AtomicWord -> [T t] -> r)
         -> F t
         -> r
foldF kneg kquant kbinop kinfix kpredapp f = foldFormula0 kneg kquant kbinop kinfix kpredapp (unwrapF f)

foldT ::
         (Copointed (Term0 (T t)) (t (Term0 (T t)))) =>
           (String -> r)
         -> (Double -> r)
         -> (V -> r)
         -> (AtomicWord -> [T t] -> r)
         -> T t
         -> r
foldT kdistinct knum kvar kfunapp t = foldTerm0 kdistinct knum kvar kfunapp (unwrapT t)
