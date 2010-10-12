module Codec.TPTP.Import(parse,parseFile
                        ,Token(..)) where
    

import Lexer
import Parser
import Parser2
import Codec.TPTP.Base
   

    
parse :: String -> [TPTP_Input]
parse = parseTPTP . map snd . alexScanTokens

parseFile :: FilePath -> IO [TPTP_Input]
parseFile x = parse `fmap` readFile x


parse2 :: String -> [TPTP_Input_C]
parse2 = parseTPTPwithComment . map snd . alexScanTokens

parse2File :: FilePath -> IO [TPTP_Input_C]
parse2File x = parse2 `fmap` readFile x

