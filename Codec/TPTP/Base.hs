{-# OPTIONS -XRecordWildCards -XCPP -XDeriveDataTypeable -XNoMonomorphismRestriction #-}

module Codec.TPTP.Base where
    
import Data.Generics
import Data.Set as S
    
        
    
data TPTP_Input = 
    -- | Annotated formula
    AFormula {
      name :: String 
    , role :: Role 
    , formula :: Formula 
    , sourceInfo :: SourceInfo 
    , usefulInfo :: UsefulInfo
    }    
    | Comment String

    deriving (Eq,Ord,Show,Read,Data,Typeable)
             
            
                   
data SourceInfo = NoSourceInfo | SourceInfo GeneralTerm
                  deriving (Eq,Ord,Show,Read,Data,Typeable)
              
data UsefulInfo = NoUsefulInfo | UsefulInfo [GeneralTerm]
                  deriving (Eq,Ord,Show,Read,Data,Typeable)
                           
data Role = Role { unrole :: String }
            deriving (Eq,Ord,Show,Read,Data,Typeable)

                   
-- | Let's make this extensible by leaving the recursion explicit
data Formula0 term formula = 
              formula :<=>: formula -- ^ Equivalence
            | formula :=>: formula -- ^ Implication
            | formula :<=: formula -- ^ Implication (reverse)
            | formula :&: formula -- ^ AND
            | formula :|: formula -- ^ OR
            | formula :~&: formula -- ^ NAND
            | formula :~|: formula -- ^ NOR
            | formula :<~>: formula -- ^ XOR
            | term :=: term -- ^ Equality
            | term :!=: term -- ^ Inequality
            | PApp String [term] -- ^ Predicate application
            | Exists [String] formula -- ^ Ex. quantification
            | All [String] formula -- ^ Univ. quantification
            | (:~:) formula -- ^ Negation
            | FromTerm term -- ^ Don't ask me, but this is required for the plain_atomic_formula and defined_plain_formula rules
              deriving (Eq,Ord,Show,Read,Data,Typeable)

-- | See 'Formula0'
data Term0 term =
            Var String
          | NumberLitTerm Double
          | DistinctObjectTerm String
          | FApp String [term] -- ^ Function symbol application (constants are nullary functions) 
            deriving (Eq,Ord,Show,Read,Data,Typeable)
                     

-- | Basic first-order formulae                   
newtype Formula = FF (Formula0 Term Formula)
    deriving (Eq,Ord,Show,Read,Data,Typeable)

-- | Basic terms
newtype Term = TT (Term0 Term)
    deriving (Eq,Ord,Show,Read,Data,Typeable)

                   
-- * Constructor wrappers
                   

x .<=>. y = FF $ x :<=>: y  
x .<~>. y = FF $ x :<~>: y  
x .=>.  y = FF $ x :=>:  y  
x .<=.  y = FF $ x :<=:  y  
x .~|.  y = FF $ x :~|:  y  
x .|.   y = FF $ x :|:   y  
x .~&.  y = FF $ x :~&:  y  
x .&.   y = FF $ x :&:   y  
(.~.) x = FF $ (:~:) x
x .=. y   = FF $ x :=:   y 
x .!=. y  = FF $ x :!=:  y 
for_all vars x = FF $ All vars x
exists vars x = FF $ Exists vars x
pApp x args = FF $ PApp x args
fromTerm = FF . FromTerm
                
var = TT . Var
fApp x args = TT $ FApp x args
numberLitTerm = TT . NumberLitTerm
distinctObjectTerm = TT . DistinctObjectTerm
                     
infixl 2 :<=>: , .<=>. , :=>: , .=>. , :<=: , .<=. , :<~>: , .<~>.
infixl 3 :|: , .|. , :~|: , .~|.
infixl 4 :&: , .&. , :~&: , .~&.
infixl 5 :=: , .=. , :!=: , .!=.

-- * Misc

class FormulaOrTerm a where
    elimFormulaOrTerm :: (Formula -> r) -> (Term -> r) -> a -> r

instance FormulaOrTerm Formula where
    elimFormulaOrTerm k _ x = k x
                              
instance FormulaOrTerm Term where
    elimFormulaOrTerm _ k x = k x

free_vars = elimFormulaOrTerm free_vars0 free_vars0

free_vars0 :: Data d => d -> Set String
free_vars0 x = case cast x :: Maybe Formula of
                Just (FF (All vars f0))    -> free_vars0 f0 `S.difference` S.fromList vars 
                Just (FF (Exists vars f0)) -> free_vars0 f0 `S.difference` S.fromList vars 
                Just (FF f)                -> unions (gmapQ free_vars0 f)
                
                otherwise ->
                  case cast x :: Maybe Term of 
                    Just (TT (Var s)) -> S.singleton s
                    Just (TT t)       -> unions (gmapQ free_vars0 t)
                    otherwise    -> S.empty
                           
-- | Universally quantify all free variables in the formula
univquant_free_vars cnf = 
    case S.toList (free_vars cnf) of
      [] -> cnf
      vars -> for_all vars cnf

data GeneralData = GWord String
                 | GApp String [GeneralTerm]
                 | GVar String
                 | GNumber Double
                 | GDistinctObject String
                 | GFormulaData 
                   deriving (Eq,Ord,Show,Read,Data,Typeable)
        
type GeneralTerm = [GeneralData]                   

-- | Since the TPTP grammar is ambiguous, predicate symbol applications are currently parsed as "fromTerm <function symbol application>" instead. Undo this.
recover_predsyms :: Formula -> Formula
recover_predsyms = go

    where
      go :: GenericT
      go = everywhere (mkT go')
             
      go' :: Formula0 Term Formula -> Formula0 Term Formula
      go' x = case x of
            FromTerm (TT (FApp sym args)) -> PApp sym args
            otherwise -> x
