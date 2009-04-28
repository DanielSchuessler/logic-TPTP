{
module Lexer where
}

%wrapper "posn"

$sign = [\+\-]
$exponent = [Ee]
$numeric = 0-9
$non_zero_numeric = 1-9
$lower_alpha = a-z
$upper_alpha = A-Z
$alpha_numeric =  [$lower_alpha$upper_alpha$numeric\_] 
$dollar = \$
$printable_char = .
$viewable_char = [$printable_char\n]
               
    
@not_star_slash = ( ([\n] | [^\*]) * ("*"+) ([\n] | [^\/\*]) )* ([\n] | [^\*])*
@sq_char = [^\\\'] | [\\][\\\']  
@do_char = [^\\\"] | [\\][\\\"] 

@decimal_natural = [0]| $non_zero_numeric $numeric* 
@signed_decimal = $sign @decimal_natural
@decimal = @signed_decimal | @decimal_natural 
@dot_decimal = "." $numeric $numeric*
@decimal_fraction = @decimal @dot_decimal
@decimal_exponent = ( @decimal | @decimal_fraction ) $exponent @decimal
               

tokens :-

  $white+                                      ;
  "("                                          { withPos $ const LP }
  "["                                          { withPos $ const Lbrack }
  "]"                                          { withPos $ const Rbrack }
  ")"                                          { withPos $ const RP }
  ","                                          { withPos $ const Comma }
  "!="|"="|"<=>"|"<="|"=>"|"<~>"|"&"|"|"
      |"~|"|"~&"|"!"|"?"|":"|"~"               { withPos $ Oper }
  "."                                          { withPos $ const Dot }
  ("%"|"#")$printable_char*                    { withPos $ CommentToken } -- comment line
  "/*" @not_star_slash "*"("*"*)"/"            { withPos $ CommentToken } -- comment block 
  [\'] @sq_char* [\']                          { withPos SingleQuoted }
  [\"] @do_char* [\"]                          { withPos DoubleQuoted }
  $dollar $dollar $lower_alpha $alpha_numeric* { withPos DollarDollarWord }
  $dollar $lower_alpha $alpha_numeric*         { withPos DollarWord }
  $upper_alpha $alpha_numeric*                 { withPos UpperWord }
  $lower_alpha $alpha_numeric*                 { withPos LowerWord }
  "*"                                          { withPos $ const Star }
  "+"                                          { withPos $ const Plus }
  ">"                                          { withPos $ const Rangle }
  @decimal_fraction | @decimal_exponent        { withPos (Real . read . stripPlus) }
  $sign @decimal_natural                       { withPos (SignedInt . read . stripPlus) }
  @decimal_natural                             { withPos (UnsignedInt . read . stripPlus) }


  
  


{
-- Each action has type :: String -> Token

withPos f pos s = (pos, f s)

-- The token type:
data Token = 
           LP 
         | RP 
         | Comma 
         | Dot 
         | Lbrack 
         | Rbrack
         | Oper String
         | SingleQuoted String
         | DoubleQuoted String
         | DollarWord String 
         | DollarDollarWord String
         | UpperWord String
         | LowerWord String
         | Star
         | Plus
         | Rangle
         | SignedInt Int
         | UnsignedInt Int
         | Real Double
         | CommentToken String
	deriving (Eq,Ord,Show)

-- alex defines: alexScanTokens

stripPlus ('+':xs) = xs
stripPlus xs = xs
}
