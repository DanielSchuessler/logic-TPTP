{
module Parser where
    
import Data.Char
import Data.Data
import Control.Monad
import Data.List as L
import Lexer
import Data.Set as S
import Codec.TPTP.Base
import System.IO
import System.IO.Unsafe
import Control.Monad.Identity
}

%name parseTPTP
%tokentype { Token }
%error { 
          
          ((\xs -> case xs of
                    xs -> error ("Parse error, pos: "++show (take 25 xs))))
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
 include_           { LowerWord "include" }
 
 single_quoted      { SingleQuoted $$ }
 distinct_object    { DoubleQuoted $$ }
 dollar_word        { DollarWord $$ }
 dollar_dollar_word { DollarDollarWord $$ }
 upper_word         { UpperWord $$ }
 lower_word         { LowerWord $$ }
 signed_integer     { SignedInt $$ }
 unsigned_integer   { UnsignedInt $$ }
 real               { Real $$ }
            
 comment            { CommentToken $$ }

     
%% 

TPTP_file  :: {[TPTP_Input]}
TPTP_file  : {[]} | TPTP_input TPTP_file  {$1 : $2}
           
TPTP_input  :: {TPTP_Input}
TPTP_input  : annotated_formula  {$1} 
             | include  { $1 }
             | comment { Comment $1 }

annotated_formula  :: {TPTP_Input}
annotated_formula  :  fof_annotated  {$1} 
                    | cnf_annotated  {$1}

fof_annotated  :: {TPTP_Input}
fof_annotated  : fof lp name  comma formula_role  comma fof_formula  annotations  rp dot
       { AFormula        $3               $5                $7           $8 }
                
cnf_annotated  :: {TPTP_Input}
cnf_annotated  : cnf lp name  comma formula_role  comma cnf_formula  annotations  rp dot
       { AFormula          $3              $5  (univquant_free_vars $7) $8 }
       
       
annotations  :: { Annotations }
annotations  :  comma source optional_info  { Annotations $2 $3 } 
    | { NoAnnotations }

formula_role  :: {Role}
formula_role  : lower_word_ { Role $1 }


fof_formula  :: {Formula}
fof_formula  : binary_formula  { $1 }
              | unitary_formula  { $1 }
      
binary_formula  :: {Formula}
binary_formula  : nonassoc_binary  {$1}
                 | assoc_binary  {$1}
                 
nonassoc_binary  :: {Formula}
nonassoc_binary  : unitary_formula  binary_connective  unitary_formula 
                  { $2 $1 $3 }

                  
assoc_binary  :: {Formula}
assoc_binary  : or_formula  { $1 }
               | and_formula  { $1 }
    
                 
or_formula  :: {Formula}
or_formula  : unitary_formula   vline  unitary_formula  more_or_formula
               { foldl (.|.) ($1 .|. $3) $4 }
             
more_or_formula  :: {[Formula]}
more_or_formula  : {[]} | vline  unitary_formula more_or_formula 
                  { $2 : $3 }
                  
and_formula  :: {Formula}
and_formula  : unitary_formula  ampersand unitary_formula  more_and_formula
               { foldl (.&.) ($1 .&. $3) $4 }


more_and_formula  :: {[Formula]}
more_and_formula  : {[]} | ampersand unitary_formula more_and_formula 
                   { $2 : $3 }
                   
unitary_formula  :: {Formula}
unitary_formula  :  quantified_formula  {$1}
                  | unary_formula       {$1}
                  | atomic_formula      {$1}
                  | lp fof_formula  rp  {$2}
                   
quantified_formula  :: {Formula}
quantified_formula  : quantifier  lbra variable_list  rbra colon unitary_formula 
                     {      $1                $3                           $6 }

variable_list  :: {[V]}
variable_list  : variable  { [$1] }
                | variable  comma variable_list  { $1 : $3 }

unary_formula  :: {Formula}
unary_formula  : unary_connective  unitary_formula  { $1 $2 }
                | fol_infix_unary  { $1 }
   



-- cnf_formula :: {Formula}
-- cnf_formula : assoc_binary {$1}
--             | lp assoc_binary rp {$2}

cnf_formula  :: {Formula}
cnf_formula  :  lp disjunction  rp  { $2 }
              | disjunction  { $1 }

                
disjunction  :: {Formula}
disjunction  : literal  more_disjunction 
              { foldl (.|.) $1 $2 }
              
more_disjunction  :: {[Formula]}
more_disjunction  :  {[]} | vline  literal more_disjunction 
                   { $2 : $3 }

literal  :: {Formula}
literal  : atomic_formula  {$1} 
          | tilde atomic_formula  { (.~.) $2} 
          | fol_infix_unary  {$1}
          
fol_infix_unary  :: {Formula}
fol_infix_unary  : term  infix_inequality  term  { $2 $1 $3 }

quantifier :: {[V] -> Formula -> Formula}
quantifier  : exclam { for_all } | question { exists }

binary_connective  :: {Formula -> Formula -> Formula}
binary_connective  : iff { (.<=>.) }
                    | implies { (.=>.) }
                    | impliedby { (.<=.) }
                    | xor { (.<~>.) }
                    | nor  { (.~|.) }
                    | nand { (.~&.) }

--- assoc_connective  : vline  
---                    | ampersand
                   
unary_connective  :: {Formula -> Formula}
unary_connective  : tilde { (.~.) }

-- defined_type  :== atomic_defined_word 

-- defined_type  :== $oType | $o | $iType | $i | $tType | $real | $int
-- system_type  :== atomic_system_word 

atomic_formula  :: {Formula} 
atomic_formula  :  plain_atomic_formula    {$1}
                 | defined_atomic_formula  {$1}
                 | system_atomic_formula   {$1}
                 

plain_atomic_formula  :: {Formula}
plain_atomic_formula  : plain_term  { fApp2pApp $1 }

-- plain_atomic_formula  :== proposition  | predicate  lp arguments  rp 
-- proposition  :== predicate 
-- predicate  :== atomic_word 

defined_atomic_formula  :: {Formula}
defined_atomic_formula  :  defined_plain_formula  {$1} 
                         | defined_infix_formula  {$1}
                         
defined_plain_formula  :: {Formula}
defined_plain_formula  : defined_plain_term  {fApp2pApp $1}
                        
--defined_plain_formula  :== defined_prop  | defined_pred  lp arguments  rp 
--defined_prop  :== atomic_defined_word 
--defined_prop  :== $true | $false
--defined_pred  :== atomic_defined_word 
--defined_pred  :== $equal


defined_infix_formula  :: {Formula}
defined_infix_formula  : term  defined_infix_pred  term  { $2 $1 $3 }
                        
defined_infix_pred :: { Term -> Term -> Formula } 
defined_infix_pred  : infix_equality  { $1 }

infix_equality  :: { Term -> Term -> Formula }
infix_equality  : equals { (.=.) }
                
infix_inequality  :: { Term -> Term -> Formula }
infix_inequality  : nequals { (.!=.) }

system_atomic_formula  :: {Formula} 
system_atomic_formula  : system_term  {fApp2pApp $1}

term  :: {Term}                        
term  :  function_term  {$1}
       | variable       {var $1}
       
function_term  :: {Term}                        
function_term  : plain_term {$1} 
                | defined_term  {$1}
                | system_term {$1}

plain_term  :: {Term}                        
plain_term  :  constant                  {fApp $1 []}
             | functor  lp arguments  rp {fApp $1 $3}
              
constant  :: {AtomicWord}
constant  : functor {$1}

functor  :: {AtomicWord}
functor  : atomic_word {$1}

defined_term  :: {Term}                        
defined_term  : defined_atom {$1} 
               | defined_atomic_term {$1}
               

defined_atom  :: {Term}                        
defined_atom  : number {numberLitTerm $1} 
               | distinct_object {distinctObjectTerm (stripQuotes '"' $1)}
                 
defined_atomic_term :: {Term}                
defined_atomic_term  : defined_plain_term {$1}
                      
defined_plain_term  :: {Term}                        
defined_plain_term  : defined_constant {fApp (AtomicWord $1) []} 
                     | defined_functor  lp arguments  rp {fApp (AtomicWord $1) $3} 

defined_constant  :: {String}
defined_constant  : defined_functor {$1}
-- defined_constant  :==

defined_functor  :: {String}
defined_functor  : atomic_defined_word {$1}
-- defined_functor  :==

system_term  :: {Term} 
system_term  :  system_constant  {fApp (AtomicWord $1) []}
              | system_functor  lp arguments  rp {fApp (AtomicWord $1) $3} 

system_constant  :: {String}                
system_constant  : system_functor  {$1}

system_functor  :: {String}                
system_functor  : atomic_system_word {$1}
                 
variable  :: {V}
variable  : upper_word {V $1}

arguments  :: {[Term]}
arguments  : term  {[$1]}
            | term  comma arguments  { $1 : $3 }

source  :: {GTerm} 
source  : general_term  {$1}

-- source  :== dag_source  | internal_source  | external_source  | unknown

-- dag_source  :== name  | inference_record 

-- inference_record  :== inference lp inference_rule  comma useful_info  comma  [parent_list ] rp 
-- inference_rule  :== atomic_word 
-- parent_list  :== parent_info  | parent_info  comma parent_list 
-- parent_info  :== source parent_details 
-- parent_details  :== :general_list  | null 
-- internal_source  :== introduced lp intro_type optional_info  rp 
-- intro_type  :== definition | axiom_of_choice | tautology | assumption
-- external_source  :== file_source  | theory  | creator_source 
-- file_source  :== file lp file_name file_info  rp 
-- file_info  :==  comma name  | null 
-- theory  :== theory lp theory_name optional_info  rp 
-- theory_name  :== equality | ac
-- creator_source  :== creator lp creator_name optional_info  rp 
-- creator_name  :== atomic_word 

optional_info  :: {UsefulInfo} 
optional_info  :  comma useful_info  {$2} |  {NoUsefulInfo}
    
useful_info  :: { UsefulInfo }
useful_info  : general_list  {UsefulInfo $1}

-- useful_info  :== [] | [info_items ]
-- info_items  :== info_item  | info_item  comma info_items 
-- info_item  :== formula_item  | inference_item  | general_function 
-- formula_item  :== description_item  | iquote_item 
-- description_item  :== description lp atomic_word  rp 
-- iquote_item  :== iquote lp atomic_word  rp 
-- inference_item  :== inference_status  | assumptions_record  | refutation 
-- inference_status  :== status lp status_value  rp  | inference_info 
-- status_value  :== suc | unp | sap | esa | sat | fsa | thm | eqv | tac | wec | eth | tau | wtc | wth | cax | sca | tca | wca |cup | csp | ecs | csa | cth | ceq | unc | wcc | ect | fun | uns | wuc | wct | scc | uca | noc

-- inference_info  :== inference_rule  lp atomic_word  comma general_list  rp 
-- assumptions_record  :== assumptions lp [name_list ] rp 
-- refutation  :== refutation lp file_source  rp 

include :: {TPTP_Input}
include  : include_ lp file_name formula_selection  rp dot { Include $3 $4 } 

formula_selection  :: {[AtomicWord]}
formula_selection  :  comma lbra name_list  rbra { $3 } 
                    |   { [] }

name_list  :: {[AtomicWord]}
name_list  : name  {[$1]}
            | name  comma name_list  { $1 : $3 }

              
general_term  :: {GTerm}
general_term  :  general_data  {GTerm $1} 
               | general_data colon general_term  {ColonSep $1 $3} 
               | general_list {GList $1} 

general_data  :: {GData}
general_data  :  atomic_word  { GWord $1 } 
               | atomic_word  lp general_terms  rp { GApp $1 $3 }  
               | variable  { GVar $1 }
               | number  { GNumber $1 }
               | distinct_object { GDistinctObject (stripQuotes '"' $1) }
               | formula_data  { $1 }
               
formula_data :: {GData}
formula_data  : dollar_word lp fof_formula  rp { GFormulaData $1 $3 }
              -- too ambiguous | dollar_word lp cnf_formula  rp { GFormulaData $1 $3 }
                
general_list  :: {[GTerm]}
general_list  : lbra rbra {[]}
               | lbra general_terms  rbra {$2}
               
general_terms  :: {[GTerm]}
general_terms  :  general_term  {[$1]} 
                | general_term  comma general_terms  {$1 : $3}

name  :: {AtomicWord}
name  : atomic_word  {$1}
       | unsigned_integer {AtomicWord(show $1)}
         
atomic_word  :: {AtomicWord}       
atomic_word  : lower_word_ {AtomicWord $1}
              | single_quoted{AtomicWord (stripQuotes '\'' $1)}
                
atomic_defined_word  :: {String}              
atomic_defined_word  : dollar_word{$1}
                      
atomic_system_word  :: {String}              
atomic_system_word  : dollar_dollar_word{$1}

number  :: {Double} -- maybe keep track of the number type that was actually parsed
number  : real {$1} | signed_integer {fromIntegral $1} | unsigned_integer {fromIntegral $1}
    
file_name  :: {String}              
file_name  : single_quoted {stripQuotes '\'' $1}
            
lower_word_ :: {String}
lower_word_ : lower_word {$1} | fof {"fof"} | cnf {"cnf"} | include_ {"include"} -- "fof" is a perfectly cromulent lower_word, but it is interpreted as a "fof" token
                               




       
{

stripQuotes which (x:xs) = go xs
                      where
                        go [x] = []
                        go ('\\':'\\':xs) = '\\':go xs
                        go ('\\':which:xs) = which:go xs
                        go (x:xs) = x:go xs
                     
fApp2pApp (T (Identity (FunApp x args))) = (F (Identity (PredApp x args))) 
}

