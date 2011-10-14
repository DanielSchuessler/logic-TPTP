{-# OPTIONS -Wall #-}
module Codec.TPTP.Import(parse,parseFile
                        ,parseWithComment,parseWithCommentFile
                        ,Token(..)) where


import Lexer
import Parser
import ParserC
import Codec.TPTP.Base


parse :: String -> [TPTP_Input]
parse = parseTPTP . map snd . alexScanTokens

parseFile :: FilePath -> IO [TPTP_Input]
parseFile x = parse `fmap` readFile x


parseWithComment :: String -> [TPTP_Input_C]
parseWithComment = parseTPTPwithComment . map snd . alexScanTokens

parseWithCommentFile :: FilePath -> IO [TPTP_Input_C]
parseWithCommentFile x = parseWithComment `fmap` readFile x

