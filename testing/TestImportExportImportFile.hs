{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Monoid
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import System.Exit
import Common
import Options.Applicative

import Codec.TPTP

data Options
  = Options
  { optPrintExport :: Bool
  , optPrintFailureOnly :: Bool
  }

optionsParser :: Parser Options
optionsParser = Options <$> printExportOption <*> printFailureOnlyOption
  where
    printExportOption = argument auto
      $  metavar "True|False"
      <> help ("print exported result")
    printFailureOnlyOption = switch
      $  long "print-failure-only"
      <> help ("print failure only")

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optionsParser) $ mconcat
  [ fullDesc
  ]

-- Note: This test expects a list of .p files (one per line) through stdin

main ::  IO ()
main = do
  files <- lines `fmap` getContents
  --print (length files)
  Options print_export print_failure_only <- execParser parserInfo
  forM_ files (diff_once_twice print_export print_failure_only)
  exitWith ExitSuccess

diff_once_twice ::  Bool -> Bool -> String -> IO ()
diff_once_twice print_export print_failure_only infilename'  = do
  --let tmp = "/tmp/tmp.tptp"
  unless print_failure_only $ putStrLn infilename'
  input <- BS.readFile infilename'
  case findUnsupportedFormulaTypeByteString input of
     Just x -> do
       unless print_failure_only $ 
         putStrLn . prettySimple . yellow . text $ ("Skipping unsupported formula type "++BS.unpack x)
     Nothing -> do

      let once = parseByteString (BL.fromStrict input)
      let tptp = toTPTPByteString once
      when (print_export && not print_failure_only) $
        BL.putStrLn $ BL.pack "new tptp = " <> tptp
      let twice = parseByteString tptp
      let dif = mconcat (zipWith diffAFormula once twice)
      let success = (putStrLn . prettySimple . dullgreen . text $ "Ok")

      -- case dif of
      --   OtherSame -> success
      --   FormulaDiff (F Same) -> success
      --   _ -> do
      --     putStrLn . prettySimple $ dif
      --     exitWith (ExitFailure 1)

      if once==twice
         then do
           unless print_failure_only $ success
         else do
           when print_failure_only $ putStrLn infilename'
           putStrLn . prettySimple $ dif
           exitWith (ExitFailure 1)
