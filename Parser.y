{
module Parser where

import Data.Char
import Data.Data
import Data.Ratio
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
 tok_lp                 { LP }
 tok_rp                 { RP }
 tok_lbra               { Lbrack }
 tok_rbra               { Rbrack }
 tok_comma              { Comma }
 tok_dot                { Dot }
 tok_star               { Star }
 tok_plus               { Plus }
 tok_rangle             { Rangle }
 tok_colon              { Oper ":" }

 tok_iff                { Oper "<=>"}
 tok_implies            { Oper "=>"}
 tok_xor                { Oper "<~>"}
 tok_nor                { Oper "~|"}
 tok_nand               { Oper "~&"}
 tok_impliedby          { Oper "<=" }
 tok_equals             { Oper "=" }
 tok_nequals            { Oper "!=" }

 tok_exclam             { Oper "!" }
 tok_question           { Oper "?" }
 tok_ampersand          { Oper "&" }
 tok_vline              { Oper "|" }
 tok_tilde              { Oper "~" }

 tok_fof                { LowerWord "fof" }
 tok_cnf                { LowerWord "cnf" }
 tok_include_           { LowerWord "include" }

 tok_fd_fof             { DollarWord "$fof" }
 tok_fd_cnf             { DollarWord "$cnf" }
 tok_fd_fot             { DollarWord "$fot" }

 tok_single_quoted      { SingleQuoted $$ }
 tok_distinct_object    { DoubleQuoted $$ }
 tok_dollar_word        { DollarWord $$ }
 tok_dollar_dollar_word { DollarDollarWord $$ }
 tok_upper_word         { UpperWord $$ }
 tok_lower_word         { LowerWord $$ }
 tok_signed_integer     { SignedInt $$ }
 tok_unsigned_integer   { UnsignedInt $$ }
 tok_real               { Real $$ }
 tok_slash              { Slash }

 comment            { CommentToken $$ }


%%

TPTP_file  :: {[TPTP_Input_ c]}
TPTP_file  : {[]} | TPTP_input TPTP_file  {$1 : $2}

TPTP_input  :: {TPTP_Input_ c}
TPTP_input  : annotated_formula  {$1}
             | include  { $1 }
             | comment { Comment $1 }

annotated_formula  :: {TPTP_Input_ c}
annotated_formula  :  fof_annotated  {$1}
                    | cnf_annotated  {$1}

fof_annotated  :: {TPTP_Input_ c}
fof_annotated  : fof lp name  comma formula_role  comma fof_formula  annotations  rp dot
       { AFormula        $3               $5                $7           $8 }

cnf_annotated  :: {TPTP_Input_ c}
cnf_annotated  : cnf lp name  comma formula_role  comma cnf_formula  annotations  rp dot
       { AFormula          $3              $5  (univquant_free_vars $7) $8 }


annotations  :: { Annotations }
annotations  :  comma source optional_info  { Annotations $2 $3 }
    | { NoAnnotations }

formula_role  :: {Role}
formula_role  : lower_word_ { Role $1 }


fof_formula  :: {F c}
fof_formula  : binary_formula  { $1 }
              | unitary_formula  { $1 }

binary_formula  :: {F c}
binary_formula  : nonassoc_binary  {$1}
                 | assoc_binary  {$1}

nonassoc_binary  :: {F c}
nonassoc_binary  : unitary_formula  binary_connective  unitary_formula
                  { $2 $1 $3 }


assoc_binary  :: {F c}
assoc_binary  : or_formula  { $1 }
               | and_formula  { $1 }


or_formula  :: {F c}
or_formula  : unitary_formula   vline  unitary_formula  more_or_formula
               { L.foldl (.|.) ($1 .|. $3) $4 }

more_or_formula  :: {[F c]}
more_or_formula  : {[]} | vline  unitary_formula more_or_formula
                  { $2 : $3 }

and_formula  :: {F c}
and_formula  : unitary_formula  ampersand unitary_formula  more_and_formula
               { L.foldl (.&.) ($1 .&. $3) $4 }


more_and_formula  :: {[F c]}
more_and_formula  : {[]} | ampersand unitary_formula more_and_formula
                   { $2 : $3 }

unitary_formula  :: {Formula}
unitary_formula  :  quantified_formula  {$1}
                  | unary_formula       {$1}
                  | atomic_formula      {$1}
                  | lp fof_formula  rp  {$2}

quantified_formula  :: {F c}
quantified_formula  : quantifier  lbra variable_list  rbra colon unitary_formula
                     { $1 $3 $6 }

variable_list  :: {[V]}
variable_list  : variable  { [$1] }
                | variable  comma variable_list  { $1 : $3 }

unary_formula  :: {F c}
unary_formula  : unary_connective  unitary_formula  { $1 $2 }
                | fol_infix_unary  { $1 }




-- cnf_formula :: {Formula}
-- cnf_formula : assoc_binary {$1}
--             | lp assoc_binary rp {$2}

cnf_formula  :: {F c}
cnf_formula  :  lp disjunction  rp  { $2 }
              | disjunction  { $1 }


disjunction  :: {F c}
disjunction  : literal  more_disjunction
              { L.foldl (.|.) $1 $2 }

more_disjunction  :: {[F c]}
more_disjunction  :  {[]} | vline  literal more_disjunction
                   { $2 : $3 }

literal  :: {Formula}
literal  : atomic_formula  {$1}
          | tilde atomic_formula  { (.~.) $2}
          | fol_infix_unary  {$1}

fol_infix_unary  :: {F c}
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

atomic_formula  :: {F c}
atomic_formula  :  plain_atomic_formula    {$1}
                 | defined_atomic_formula  {$1}
                 | system_atomic_formula   {$1}


plain_atomic_formula  :: {F c}
plain_atomic_formula  : plain_term  { fApp2pApp $1 }

-- plain_atomic_formula  :== proposition  | predicate  lp arguments  rp
-- proposition  :== predicate
-- predicate  :== atomic_word

defined_atomic_formula  :: {F c}
defined_atomic_formula  :  defined_plain_formula  {$1}
                         | defined_infix_formula  {$1}

defined_plain_formula  :: {F c}
defined_plain_formula  : defined_plain_term  {fApp2pApp $1}

--defined_plain_formula  :== defined_prop  | defined_pred  lp arguments  rp
--defined_prop  :== atomic_defined_word
--defined_prop  :== $true | $false
--defined_pred  :== atomic_defined_word
--defined_pred  :== $equal


defined_infix_formula  :: {F c}
defined_infix_formula  : term  defined_infix_pred  term  { $2 $1 $3 }

defined_infix_pred :: { T c -> T c -> F c }
defined_infix_pred  : infix_equality  { $1 }

infix_equality  :: { Term -> Term -> Formula }
infix_equality  : equals { (.=.) }

infix_inequality  :: { Term -> Term -> Formula }
infix_inequality  : nequals { (.!=.) }

system_atomic_formula  :: {F c}
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

arguments  :: {[T c]}
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
               | formula_data  { $1 }
               | atomic_word  lp general_terms  rp { GApp $1 $3 }
               | variable  { GVar $1 }
               | number  { GNumber $1 }
               | distinct_object { GDistinctObject (stripQuotes '"' $1) }


formula_data :: {GData}
formula_data  :  tok_fd_fof lp fof_formula  rp { GFormulaData "$fof" $3 }
               | tok_fd_cnf lp cnf_formula  rp { GFormulaData "$cnf" $3 }
               | tok_fd_fot lp term rp         { GFormulaTerm "$fot" $3 }

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

number  :: {Rational} -- maybe keep track of the number type that was actually parsed
number  : integer {fromIntegral $1} | rational {$1} | real {$1}

integer :: {Integer}
integer : signed_integer {$1} | unsigned_integer {$1}

rational :: {Rational}
rational : integer tok_slash unsigned_integer {$1 % $3}

file_name  :: {String}
file_name  : single_quoted {stripQuotes '\'' $1}

lower_word_ :: {String}
lower_word_ : lower_word {$1} | fof {"fof"} | cnf {"cnf"} | include_ {"include"} -- "fof" is a perfectly cromulent lower_word, but it is interpreted as a "fof" token

lp                 :: {Token}
lp                 : tok_lp                  comment_list { $1 }
rp                 :: {Token}
rp                 : tok_rp                  comment_list { $1 }
lbra               :: {Token}
lbra               : tok_lbra                comment_list { $1 }
rbra               :: {Token}
rbra               : tok_rbra                comment_list { $1 }
comma              :: {Token}
comma              : tok_comma               comment_list { $1 }
dot                :: {Token}
dot                : tok_dot                 comment_list { $1 }
star               :: {Token}
star               : tok_star                comment_list { $1 }
plus               :: {Token}
plus               : tok_plus                comment_list { $1 }
rangle             :: {Token}
rangle             : tok_rangle              comment_list { $1 }
colon              :: {Token}
colon              : tok_colon               comment_list { $1 }

iff                :: {Token}
iff                : tok_iff                 comment_list { $1 }
implies            :: {Token}
implies            : tok_implies             comment_list { $1 }
xor                :: {Token}
xor                : tok_xor                 comment_list { $1 }
nor                :: {Token}
nor                : tok_nor                 comment_list { $1 }
nand               :: {Token}
nand               : tok_nand                comment_list { $1 }
impliedby          :: {Token}
impliedby          : tok_impliedby           comment_list { $1 }
equals             :: {Token}
equals             : tok_equals              comment_list { $1 }
nequals            :: {Token}
nequals            : tok_nequals             comment_list { $1 }

exclam             :: {Token}
exclam             : tok_exclam              comment_list { $1 }
question           :: {Token}
question           : tok_question            comment_list { $1 }
ampersand          :: {Token}
ampersand          : tok_ampersand           comment_list { $1 }
vline              :: {Token}
vline              : tok_vline               comment_list { $1 }
tilde              :: {Token}
tilde              : tok_tilde               comment_list { $1 }

fof                :: {Token}
fof                : tok_fof                 comment_list { $1 }
cnf                :: {Token}
cnf                : tok_cnf                 comment_list { $1 }
include_           :: {Token}
include_           : tok_include_            comment_list { $1 }

single_quoted      :: {String}
single_quoted      : tok_single_quoted       comment_list { $1 }
distinct_object    :: {String}
distinct_object    : tok_distinct_object     comment_list { $1 }
dollar_word        :: {String}
dollar_word        : tok_dollar_word         comment_list { $1 }
dollar_dollar_word :: {String}
dollar_dollar_word : tok_dollar_dollar_word  comment_list { $1 }
upper_word         :: {String}
upper_word         : tok_upper_word          comment_list { $1 }
lower_word         :: {String}
lower_word         : tok_lower_word          comment_list { $1 }
signed_integer     :: {Integer}
signed_integer     : tok_signed_integer      comment_list { $1 }
unsigned_integer   :: {Integer}
unsigned_integer   : tok_unsigned_integer    comment_list { $1 }
real               :: {Rational}
real               : tok_real                comment_list { $1 }

comment_list :: {[String]}
comment_list : {[]} | comment comment_list { $1 : $2 }

{

stripQuotes which (x:xs) = go xs
                      where
                        go [x] = []
                        go ('\\':'\\':xs) = '\\':go xs
                        go ('\\':which:xs) = which:go xs
                        go (x:xs) = x:go xs

fApp2pApp (T (Identity (FunApp x args))) = (F (Identity (PredApp x args)))
}
