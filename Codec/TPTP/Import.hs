module Codec.TPTP.Import(parse,parseFile
                        ,module Codec.TPTP.Base
                        ,Token(..)) where
    

import Lexer
import Parser
import Codec.TPTP.Base
   

    
parse :: String -> [TPTP_Input]
parse = parseTPTP . map snd . alexScanTokens
        
        
parseFile :: FilePath -> IO [TPTP_Input]
parseFile x = parse `fmap` readFile x
