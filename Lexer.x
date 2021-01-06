{
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
{-# OPTIONS -fno-warn-unused-matches #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Lexer where
import Data.Ratio
import qualified Data.ByteString.Lazy.Char8 as BL
}

%wrapper "posn-bytestring"

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
      |"~|"|"~&"|"!"|"?"|":"|"~"               { withPos Oper }
  "."                                          { withPos $ const Dot }
  ("%"|"#")$printable_char*                    { withPos CommentToken } -- comment line
  "/*" @not_star_slash "*"("*"*)"/"            { withPos CommentToken } -- comment block
  [\'] @sq_char* [\']                          { withPos SingleQuoted }
  [\"] @do_char* [\"]                          { withPos DoubleQuoted }
  $dollar $dollar $lower_alpha $alpha_numeric* { withPos DollarDollarWord }
  $dollar $lower_alpha $alpha_numeric*         { withPos DollarWord }
  $upper_alpha $alpha_numeric*                 { withPos UpperWord }
  $lower_alpha $alpha_numeric*                 { withPos LowerWord }
  "*"                                          { withPos $ const Star }
  "+"                                          { withPos $ const Plus }
  ">"                                          { withPos $ const Rangle }
  @decimal_fraction | @decimal_exponent        { withPos (Real . readDecimalFraction . stripPlus) }
  $sign @decimal_natural                       { withPos (SignedInt . readInteger . stripPlus) }
  @decimal_natural                             { withPos (UnsignedInt . readInteger . stripPlus) }
  "/"                                          { withPos $ const Slash }






{
-- Each action has type :: BL.ByteString -> Token

withPos f pos s = (pos, f s)

-- The token type:
data Token =
           LP
         | RP
         | Comma
         | Dot
         | Lbrack
         | Rbrack
         | Oper BL.ByteString
         | SingleQuoted BL.ByteString
         | DoubleQuoted BL.ByteString
         | DollarWord BL.ByteString
         | DollarDollarWord BL.ByteString
         | UpperWord BL.ByteString
         | LowerWord BL.ByteString
         | Star
         | Plus
         | Rangle
         | SignedInt Integer
         | UnsignedInt Integer
         | Real Rational
         | CommentToken BL.ByteString
         | Slash
    deriving (Eq,Ord,Show)

-- alex defines: alexScanTokens

stripPlus :: BL.ByteString -> BL.ByteString
stripPlus xs =
    case BL.uncons xs of
        Just ('+', xs') -> xs'
        _ -> xs

readDecimalFraction :: BL.ByteString -> Rational
readDecimalFraction cs =
    case BL.uncons cs of
        Just ('-', cs') -> -(readUnsignedDecimalFraction cs')
        _ -> readUnsignedDecimalFraction cs

readInteger :: BL.ByteString -> Integer
readInteger s =
    case BL.readInteger s of
        Nothing -> error "no parse"
        Just (i, _) -> i

readUnsignedDecimalFraction :: BL.ByteString -> Rational
readUnsignedDecimalFraction cs =
    case BL.break (=='.') cs of
         (_, BL.uncons -> Nothing) ->
             case breakExponent cs of
                 (cs2, BL.uncons -> Just (_, cs2')) -> readIntegerRat cs2 * readExponent cs2'              
         (cs1, BL.uncons -> Just (_, cs1')) ->
             case breakExponent cs1' of
                 (_, BL.uncons -> Nothing) -> readIntegerRat cs1 + readFraction cs1'
                 (cs2, BL.uncons -> Just (_, cs2')) -> (readIntegerRat cs1 + readFraction cs2) * readExponent cs2'
  where
    breakExponent = BL.break (`elem` "Ee")

    readExponent :: BL.ByteString -> Rational
    readExponent = (10^^) . readInteger . stripPlus

    readFraction :: BL.ByteString -> Rational
    readFraction cs = readInteger cs % (10^(BL.length cs))

    readIntegerRat :: BL.ByteString -> Rational
    readIntegerRat = fromIntegral . readInteger



}
