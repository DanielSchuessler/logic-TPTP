{-# OPTIONS -fwarn-missing-signatures -XRecordWildCards -XCPP -XDeriveDataTypeable -fglasgow-exts -XNoMonomorphismRestriction -XTemplateHaskell -XUndecidableInstances -XGeneralizedNewtypeDeriving -Wall #-}

module Codec.TPTP.Base where
    
import Data.Generics
import Data.Set as S hiding(fold)
import Control.Applicative
--import Data.Foldable
import Prelude --hiding(concat,foldl,foldl1,foldr,foldr1)
--import Data.Foldable 
--import Test.QuickCheck.Instances
import Test.QuickCheck
import Data.Char
import Control.Monad
import Codec.TPTP.QuickCheck
import Data.String
import Data.Monoid hiding(All)
    
-- * Basic undecorated formulae and terms
                   
-- | Basic (undecorated) first-order formulae                   
newtype Formula = FF (Formula0 Term Formula)
    deriving (Eq,Ord,Show,Read,Data,Typeable)

-- | Basic (undecorated) terms
newtype Term = TT (Term0 Term)
    deriving (Eq,Ord,Show,Read,Data,Typeable)

(.<=>.) :: Formula -> Formula -> Formula
x .<=>. y = FF $ BinOp  x (:<=>:) y  
(.<~>.) :: Formula -> Formula -> Formula
x .<~>. y = FF $ BinOp  x (:<~>:) y  
(.=>.) :: Formula -> Formula -> Formula
x .=>.  y = FF $ BinOp  x (:=>:)  y  
(.<=.) :: Formula -> Formula -> Formula
x .<=.  y = FF $ BinOp  x (:<=:)  y  
(.~|.) :: Formula -> Formula -> Formula
x .~|.  y = FF $ BinOp  x (:~|:)  y  
(.|.) :: Formula -> Formula -> Formula
x .|.   y = FF $ BinOp  x (:|:)   y  
(.~&.) :: Formula -> Formula -> Formula
x .~&.  y = FF $ BinOp  x (:~&:)  y  
(.&.) :: Formula -> Formula -> Formula
x .&.   y = FF $ BinOp  x (:&:)   y  
            
(.~.) :: Formula -> Formula
(.~.) x = FF $ (:~:) x
          
(.=.) :: Term -> Term -> Formula
x .=. y   = FF $ InfixPred x (:=:)   y 
(.!=.) :: Term -> Term -> Formula
x .!=. y  = FF $ InfixPred x (:!=:) y 
            
for_all :: [String] -> Formula -> Formula
for_all vars x = FF $ Quant All vars x
                 
exists :: [String] -> Formula -> Formula
exists vars x = FF $ Quant Exists vars x
                
pApp :: AtomicWord -> [Term] -> Formula
pApp x args = FF $ PredApp x args
              
var :: String -> Term
var = TT . Var
fApp :: AtomicWord -> [Term] -> Term
fApp x args = TT $ FunApp x args
numberLitTerm :: Double -> Term
numberLitTerm = TT . NumberLitTerm
distinctObjectTerm :: String -> Term
distinctObjectTerm = TT . DistinctObjectTerm
                     
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
            | Quant Quant [String] formula -- ^ Quantified formula
            | (:~:) formula -- ^ Negation
              deriving (Eq,Ord,Show,Read,Data,Typeable)
                       
                       
-- | See <http://haskell.org/haskellwiki/Indirect_composite> for the point of the type parameters (they allow for future decorations). If you don't need decorations, you can just use 'Term' and the wrapped constructors above.
data Term0 term =
            Var String -- ^ Variable
          | NumberLitTerm Double -- ^ Number literal
          | DistinctObjectTerm String -- ^ Double-quoted item
          | FunApp AtomicWord [term] -- ^ Function symbol application (constants are nullary functions) 
            deriving (Eq,Ord,Show,Read,Data,Typeable)
                     
-- | Binary formula connectives 
data BinOp =
    -- Please don't change the constructor names
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
    -- Please don't change the constructor names
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
    , sourceInfo :: SourceInfo 
    , usefulInfo :: UsefulInfo
    }    
    | Comment String
    | Include FilePath [AtomicWord]

    deriving (Eq,Ord,Show,Read,Data,Typeable)
             
             
            

-- | Annotations about the formulas origin                   
data SourceInfo = NoSourceInfo | SourceInfo GTerm
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
                 | GVar String
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

class FormulaOrTerm a where
    elimFormulaOrTerm :: (Formula -> r) -> (Term -> r) -> a -> r

instance FormulaOrTerm Formula where
    elimFormulaOrTerm k _ x = k x
                              
instance FormulaOrTerm Term where
    elimFormulaOrTerm _ k x = k x

-- | Get the free variables
free_vars :: forall a. (FormulaOrTerm a) => a -> Set String
free_vars = elimFormulaOrTerm free_vars0 free_vars0
                           
-- | Universally quantify all free variables in the formula
univquant_free_vars :: Formula -> Formula
univquant_free_vars cnf = 
    case S.toList (free_vars cnf) of
      [] -> cnf
      vars -> for_all vars cnf
             
-- ** Internal

free_vars0 :: Data d => d -> Set String
free_vars0 x = case cast x :: Maybe Formula of
                Just (FF (Quant All vars f0))    -> free_vars0 f0 `S.difference` S.fromList vars 
                Just (FF (Quant Exists vars f0)) -> free_vars0 f0 `S.difference` S.fromList vars 
                Just (FF f)                -> unions (gmapQ free_vars0 f)
                
                _ ->
                  case cast x :: Maybe Term of 
                    Just (TT (Var s)) -> S.singleton s
                    Just (TT t)       -> unions (gmapQ free_vars0 t)
                    _    -> S.empty
                                   
                                   



--- modified derive-generated code
--- have this in this module to avoid orphan instances

instance Arbitrary TPTP_Input
    where arbitrary = frequency [(10,       
                                    do 
                                      x1 <- AtomicWord <$> arbLowerWord
                                      x2 <- arbitrary
                                      x3 <- arbitrary
                                      x4 <- arbitrary
                                      x5 <- arbitrary
                                      return (AFormula x1 x2 x3 x4 x5))
                                         
                                  , (1,
                                    do 
                                      x1 <- arbPrintable
                                      return (Comment ("% "++x1))
                                  )

                                  , (1, Include `fmap` arbLowerWord `ap` 
                                         listOf arbitrary)
                                ]

instance Arbitrary Formula
    where arbitrary = fmap FF arbitrary

instance Arbitrary Term
    where arbitrary = fmap TT arbitrary

instance Arbitrary SourceInfo
    where arbitrary = oneof [
                              return NoSourceInfo
                            , do 
                                x1 <- arbitrary
                                return (SourceInfo x1)
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
                               x2 <- liftM2 (:) arbVar (argsFreq (\nargs -> vectorOf nargs arbVar))
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


            go 0 = frequency [ (2,Var <$> arbVar), (1,FunApp `fmap` arbitrary `ap` return[] ) ]

            go i = oneof [
                             do 
                              x1 <- arbVar
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
                                    , fmap GVar arbVar
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
                                     
                           ,GVar <$> arbVar
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
                            
-- | Tip: Use the @-XOverloadedStrings@ compiler flag if you don't want to type /AtomicWord/ to construct an 'AtomicWord' 
newtype AtomicWord = AtomicWord String
    deriving (Eq,Ord,Show,Data,Typeable,Read,Monoid,IsString)
                                         
instance Arbitrary AtomicWord where
    arbitrary = frequency [  (5, AtomicWord <$> arbLowerWord)
                            ,(1, AtomicWord <$> arbPrintable)
                          ]

             
-- * Fixed-point style decorated formulae and terms

-- | For a given type constructor @f@, make the fixed point type @Y@ satisfying: 
--
-- > Y = f (Term0 Y)
--
-- (modulo newtype wrapping). See for example 'diffFormula'.
newtype TermFix f = TermFix { runTermFix :: f (Term0 (TermFix f)) }
    
-- | For a given type constructor @f@, make the fixed point type @X@ satisfying: 
--
-- > X = f (Formula0 Y X) 
-- > Y = f (Term0 Y)
--
-- (modulo newtype wrapping). See for example 'diffTerm'.
newtype FormulaFix f = FormulaFix { runFormulaFix :: f (Formula0 (TermFix f) (FormulaFix f)) }
             
    
