{-# OPTIONS -Wall #-}
module Codec.TPTP.Import(parse,parseByteString,parseFile
                        ,parseWithComment,parseWithCommentByteString,parseWithCommentFile
                        ,Token(..)) where


import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Builder as Builder
import Lexer
import Parser
import ParserC
import Codec.TPTP.Base


parse :: String -> [TPTP_Input]
parse = parseByteString . Builder.toLazyByteString . Builder.stringUtf8

parseByteString :: BL.ByteString -> [TPTP_Input]
parseByteString = parseTPTP . map snd . alexScanTokens

parseFile :: FilePath -> IO [TPTP_Input]
parseFile x = parseByteString `fmap` BL.readFile x


parseWithComment :: String -> [TPTP_Input_C]
parseWithComment = parseWithCommentByteString . Builder.toLazyByteString . Builder.stringUtf8

parseWithCommentByteString :: BL.ByteString -> [TPTP_Input_C]
parseWithCommentByteString = parseTPTPwithComment . map snd . alexScanTokens

parseWithCommentFile :: FilePath -> IO [TPTP_Input_C]
parseWithCommentFile x = parseWithCommentByteString `fmap` BL.readFile x
