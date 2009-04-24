{
{-# OPTIONS -XDeriveDataTypeable #-}
--module Codec.TPTP.Import.Parser where
    
import Data.Char
import Data.Data
import Control.Monad
import Data.List as L
import Lexer
import Data.Set as S
}

%name parseTPTP
%tokentype { Token }
%error { 
          
          ((\xs -> case xs of
                    xs -> error ("Parse error, pos: "++show (take 10 xs))))
       }


%token
 lp                 { LP }
 rp                 { RP }
 lbra               { Lbrack }
 rbra               { Rbrack }
 comma              { Comma }
 dot                { Dot }
 star               { Star }
 plus               { Plus }
 rangle             { Rangle }
 colon              { Oper ":" }

 iff                { Oper "<=>"}
 implies            { Oper "=>"}
 xor                { Oper "<~>"}
 nor                { Oper "~|"}
 nand               { Oper "~&"}
 impliedby          { Oper "<=" }
 equals             { Oper "=" }
 nequals            { Oper "!=" }
         
 exclam             { Oper "!" }
 question           { Oper "?" }
 ampersand          { Oper "&" }
 vline              { Oper "|" }
 tilde              { Oper "~" }
            
 fof                { LowerWord "fof" }
 cnf                { LowerWord "cnf" }
 include            { LowerWord "include" }
 
 single_quoted      { SingleQuoted $$ }
 distinct_object    { DoubleQuoted $$ }
 dollar_word        { DollarWord $$ }
 dollar_dollar_word { DollarDollarWord $$ }
 upper_word         { UpperWord $$ }
 lower_word         { LowerWord $$ }
 signed_integer     { SignedInt $$ }
 unsigned_integer   { UnsignedInt $$ }
 real               { Real $$ }
            

     
%% 

many(p) : {[]} 
        | p many(p) { $1 : $2 }

'TPTP_file' :: {[TPTP_Input]}
'TPTP_file' : many('TPTP_input') {$1}
           
'TPTP_input' :: {TPTP_Input}
'TPTP_input' : 'annotated_formula' {$1} 
             | 'include' { error "support for 'include' not implemented" }

'annotated_formula' :: {TPTP_Input}
'annotated_formula' : 'fof_annotated' {$1} 
                    | 'cnf_annotated' {$1}

'fof_annotated' :: {TPTP_Input}
'fof_annotated' : fof lp 'name' comma 'formula_role' comma 'fof_formula' 'annotations' rp dot
       { AFormula          $3               $5                  $7      (fst $8) (snd $8) }
                
'cnf_annotated' :: {TPTP_Input}
'cnf_annotated' : cnf lp 'name' comma 'formula_role' comma 'cnf_formula' 'annotations' rp dot
       { AFormula          $3              $5             (cnf_to_fof $7)(fst $8)(snd $8) }
       
       
'annotations' :: { (SourceInfo,UsefulInfo) }
'annotations' :  comma 'source''optional_info' { ($2,$3) } 
    | 'null' { (NoSourceInfo, NoUsefulInfo) }

'formula_role' :: {Role}
'formula_role' : lower_word { Role $1 }


'fof_formula' :: {Formula}
'fof_formula' : 'binary_formula' { $1 }
              | 'unitary_formula' { $1 }
      
'binary_formula' :: {Formula}
'binary_formula' : 'nonassoc_binary' {$1}
                 | 'assoc_binary' {$1}
                 
'nonassoc_binary' :: {Formula}
'nonassoc_binary' : 'unitary_formula' 'binary_connective' 'unitary_formula'
                  { $2 $1 $3 }

                  
'assoc_binary' :: {Formula}
'assoc_binary' : 'or_formula' { $1 }
               | 'and_formula' { $1 }
    
                 
'or_formula' :: {Formula}
'or_formula' : 'unitary_formula'  vline  'unitary_formula' many('more_or_formula')
               { foldl (.|.) ($1 .|. $3) $4 }
             
'more_or_formula' :: {Formula}
'more_or_formula' :  vline  'unitary_formula'
                  { $2 }
                  
'and_formula' :: {Formula}
'and_formula' : 'unitary_formula' ampersand 'unitary_formula' many('more_and_formula')
               { foldl (.&.) ($1 .&. $3) $4 }


'more_and_formula' :: {Formula}
'more_and_formula' : ampersand 'unitary_formula'
                   { $2 }
                   
'unitary_formula' :: {Formula}
'unitary_formula' : 'quantified_formula' {$1}
                  | 'unary_formula' {$1}
                  | 'atomic_formula' {$1}
                  |  lp 'fof_formula' rp {$2}
                   
'quantified_formula' :: {Formula}
'quantified_formula' : 'quantifier' lbra 'variable_list' rbra colon 'unitary_formula'
                     {      $1                $3                           $6 }

'variable_list' :: {[String]}
'variable_list' : 'variable' { [$1] }
                | 'variable' comma 'variable_list' { $1 : $3 }

'unary_formula' :: {Formula}
'unary_formula' : 'unary_connective' 'unitary_formula' { $1 $2 }
                | 'fol_infix_unary' { $1 }
                
'cnf_formula' :: {Formula}
'cnf_formula' :  lp 'disjunction' rp  { $2 }
              | 'disjunction' { $1 }
              
'disjunction' :: {Formula}
'disjunction' : 'literal' many('more_disjunction') 
              { foldl (.|.) $1 $2 }
              
'more_disjunction' :: {Formula}
'more_disjunction' :  vline  'literal'
                   { $2 }

'literal' :: {Formula}
'literal' : 'atomic_formula' {$1} 
          | tilde 'atomic_formula' { (.~.) $2} 
          | 'fol_infix_unary' {$1}
          
'fol_infix_unary' :: {Formula}
'fol_infix_unary' : 'term' 'infix_inequality' 'term' { $2 $1 $3 }


'quantifier' : exclam { for_all } | question { exists }

'binary_connective' :: {Formula -> Formula -> Formula}
'binary_connective' : iff { (.<=>.) }
                    | implies { (.=>.) }
                    | impliedby { (.<=.) }
                    | xor { (.<~>.) }
                    | nor  { (.~|.) }
                    | nand { (.~&.) }

--- 'assoc_connective' : vline  
---                    | ampersand
                   
'unary_connective' :: {Formula -> Formula}
'unary_connective' : tilde { .~. }

-- 'defined_type' :== 'atomic_defined_word'

-- 'defined_type' :== $oType | $o | $iType | $i | $tType | $real | $int
-- 'system_type' :== 'atomic_system_word'

'atomic_formula' :: {Formula} 
'atomic_formula' : 'plain_atomic_formula' {$1}
                 | 'defined_atomic_formula' {$1}
                 | 'system_atomic_formula' {$1}
                 

'plain_atomic_formula' :: {Formula}
'plain_atomic_formula' : 'plain_term' {fromTerm $1}

-- 'plain_atomic_formula' :== 'proposition' | 'predicate' lp 'arguments' rp 
-- 'proposition' :== 'predicate'
-- 'predicate' :== 'atomic_word'

'defined_atomic_formula' :: {Formula}
'defined_atomic_formula' : 'defined_plain_formula' {$1} 
                         | 'defined_infix_formula' {$1}
                         
'defined_plain_formula' :: {Formula}
'defined_plain_formula' : 'defined_plain_term' {fromTerm $1}
                        
--'defined_plain_formula' :== 'defined_prop' | 'defined_pred' lp 'arguments' rp 
--'defined_prop' :== 'atomic_defined_word'
--'defined_prop' :== $true | $false
--'defined_pred' :== 'atomic_defined_word'
--'defined_pred' :== $equal


'defined_infix_formula' :: {Formula}
'defined_infix_formula' : 'term' 'defined_infix_pred' 'term' { $2 $1 $3 }
                        
'defined_infix_pred' : 'infix_equality' { $1 }

'infix_equality' : equals { (.=.) }
'infix_inequality' : nequals { (.!=.) }

'system_atomic_formula' :: {Formula} 
'system_atomic_formula' : 'system_term' {fromTerm $1}

'term' :: {Term}                        
'term' : 'function_term' {$1}
       | 'variable' {var $1}
       
'function_term' :: {Term}                        
'function_term' : 'plain_term'{$1} 
                | 'defined_term' {$1}
                | 'system_term'{$1}

'plain_term' :: {Term}                        
'plain_term' : 'constant' {fApp $1 []}
             | 'functor' lp 'arguments' rp {fApp $1 $3}
              
'constant' :: {String}
'constant' : 'functor'{$1}

'functor' :: {String}
'functor' : 'atomic_word'{$1}

'defined_term' :: {Term}                        
'defined_term' : 'defined_atom'{$1} 
               | 'defined_atomic_term'{$1}
               

'defined_atom' :: {Term}                        
'defined_atom' : 'number'{numberLitTerm $1} 
               | distinct_object {distinctObjectTerm $1}
               
'defined_atomic_term' : 'defined_plain_term'{$1}
                      
'defined_plain_term' :: {Term}                        
'defined_plain_term' : 'defined_constant'{fApp $1 []} 
                     | 'defined_functor' lp 'arguments' rp {fApp $1 $3} 

'defined_constant' :: {String}
'defined_constant' : 'defined_functor'{$1}
-- 'defined_constant' :==

'defined_functor' :: {String}
'defined_functor' : 'atomic_defined_word'{$1}
-- 'defined_functor' :==

'system_term' :: {Term} 
'system_term' : 'system_constant' {fApp $1 []}
              | 'system_functor' lp 'arguments' rp {fApp $1 $3} 

'system_constant' :: {String}                
'system_constant' : 'system_functor' {$1}

'system_functor' :: {String}                
'system_functor' : 'atomic_system_word'{$1}
                 
'variable' :: {String}
'variable' : upper_word {$1}

'arguments' :: {[Term]}
'arguments' : 'term' {[$1]}
            | 'term' comma 'arguments' { $1 : $3 }

'source' :: {SourceInfo} 
'source' : 'general_term' {SourceInfo $1}

-- 'source' :== 'dag_source' | 'internal_source' | 'external_source' | unknown

-- 'dag_source' :== 'name' | 'inference_record'

-- 'inference_record' :== inference lp 'inference_rule' comma 'useful_info' comma  ['parent_list'] rp 
-- 'inference_rule' :== 'atomic_word'
-- 'parent_list' :== 'parent_info' | 'parent_info' comma 'parent_list'
-- 'parent_info' :== 'source''parent_details'
-- 'parent_details' :== :'general_list' | 'null'
-- 'internal_source' :== introduced lp 'intro_type''optional_info' rp 
-- 'intro_type' :== definition | axiom_of_choice | tautology | assumption
-- 'external_source' :== 'file_source' | 'theory' | 'creator_source'
-- 'file_source' :== file lp 'file_name''file_info' rp 
-- 'file_info' :==  comma 'name' | 'null'
-- 'theory' :== theory lp 'theory_name''optional_info' rp 
-- 'theory_name' :== equality | ac
-- 'creator_source' :== creator lp 'creator_name''optional_info' rp 
-- 'creator_name' :== 'atomic_word'

'optional_info' :: {UsefulInfo} 
'optional_info' :  comma 'useful_info' {$2} | 'null' {NoUsefulInfo}
    
'useful_info' :: { UsefulInfo }
'useful_info' : 'general_list' {UsefulInfo $1}

-- 'useful_info' :== [] | ['info_items']
-- 'info_items' :== 'info_item' | 'info_item' comma 'info_items'
-- 'info_item' :== 'formula_item' | 'inference_item' | 'general_function'
-- 'formula_item' :== 'description_item' | 'iquote_item'
-- 'description_item' :== description lp 'atomic_word' rp 
-- 'iquote_item' :== iquote lp 'atomic_word' rp 
-- 'inference_item' :== 'inference_status' | 'assumptions_record' | 'refutation'
-- 'inference_status' :== status lp 'status_value' rp  | 'inference_info'
-- 'status_value' :== suc | unp | sap | esa | sat | fsa | thm | eqv | tac | wec | eth | tau | wtc | wth | cax | sca | tca | wca |cup | csp | ecs | csa | cth | ceq | unc | wcc | ect | fun | uns | wuc | wct | scc | uca | noc

-- 'inference_info' :== 'inference_rule' lp 'atomic_word' comma 'general_list' rp 
-- 'assumptions_record' :== assumptions lp ['name_list'] rp 
-- 'refutation' :== refutation lp 'file_source' rp 

'include' : include lp 'file_name''formula_selection' rp dot { error "Sorry, 'include' not implemented" } 

'formula_selection' :  comma lbra 'name_list' rbra { undefined } 
                    | 'null' { undefined }

'name_list' :: {[String]}
'name_list' : 'name' {[$1]}
            | 'name' comma 'name_list' { $1 : $3 }

              
'general_term' :: {[GeneralData]}
'general_term' : 'general_data' {[$1]} 
               | 'general_data' colon 'general_term' {$1 : $3} 
               | {[]}
               
-- 'general_list'

'general_data' :: {GeneralData}
'general_data' : 'atomic_word' { GWord $1 } 
               | 'atomic_word' lp 'general_terms' rp { GApp $1 $3 }  
               | 'variable' { GVar $1 }
               | 'number' { GNumber $1 }
               | distinct_object { GDistinctObject $1 }
               | 'formula_data' { $1 }
               
'formula_data' : dollar_word lp 'fof_formula' rp { error "formula_data not implemented" }
               | dollar_word lp 'cnf_formula' rp { error "formula_data not implemented" }
                
'general_list' :: {[GeneralTerm]}
'general_list' : lbra rbra {[]}
               | lbra 'general_terms' rbra {$2}
               
'general_terms' :: {[GeneralTerm]}
'general_terms' : 'general_term' {[]} 
                | 'general_term' comma 'general_terms' {$1 : $3}

'name' :: {String}
'name' : 'atomic_word' {$1}
       | unsigned_integer {show $1}
         
'atomic_word' :: {String}       
'atomic_word' : lower_word {$1}
              | single_quoted{$1}
                
'atomic_defined_word' :: {String}              
'atomic_defined_word' : dollar_word{$1}
                      
'atomic_system_word' :: {String}              
'atomic_system_word' : dollar_dollar_word{$1}

'number' :: {Double} -- maybe keep track of the number type that was actually parsed
'number' : real {$1} | signed_integer {fromIntegral $1} | unsigned_integer {fromIntegral $1}
    
'file_name' :: {String}              
'file_name' : single_quoted {$1}
            
'null' : {()}





       
{

data TPTP_Input = 
    -- | Annotated formula
    AFormula {
      name :: String 
    , role :: Role 
    , formula :: Formula 
    , sourceInfo :: SourceInfo 
    , usefulInfo :: UsefulInfo
    }    
    deriving (Eq,Ord,Show,Read,Data,Typeable)
                   
data SourceInfo = NoSourceInfo | SourceInfo GeneralTerm
                  deriving (Eq,Ord,Show,Read,Data,Typeable)
              
data UsefulInfo = NoUsefulInfo | UsefulInfo [GeneralTerm]
                  deriving (Eq,Ord,Show,Read,Data,Typeable)
                           
data Role = Role String
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
newtype Formula = Formula (Formula0 Term Formula)
    deriving (Eq,Ord,Show,Read,Data,Typeable)

-- | Basic terms
newtype Term = Term (Term0 Term)
    deriving (Eq,Ord,Show,Read,Data,Typeable)

                   
-- * Constructor wrappers
                   
#define FF Formula
#define TT Term

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

free_vars :: Data d => d -> Set String
free_vars x = case cast x :: Maybe Formula of
                Just (FF (All vars f0))    -> free_vars f0 `S.difference` S.fromList vars 
                Just (FF (Exists vars f0)) -> free_vars f0 `S.difference` S.fromList vars 
                Just (FF f)                -> unions (gmapQ free_vars f)
                
                otherwise ->
                  case cast x :: Maybe Term of 
                    Just (TT (Var s)) -> S.singleton s
                    Just (TT t)       -> unions (gmapQ free_vars t)
                    otherwise    -> S.empty
                           

cnf_to_fof cnf = 
    let
        vars = S.toList (free_vars cnf)
    in
      for_all vars cnf

data GeneralData = GWord String
                 | GApp String [GeneralTerm]
                 | GVar String
                 | GNumber Double
                 | GDistinctObject String
                 | GFormulaData 
                   deriving (Eq,Ord,Show,Read,Data,Typeable)
        
type GeneralTerm = [GeneralData]                   
                   
}

