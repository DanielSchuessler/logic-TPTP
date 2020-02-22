{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE PackageImports #-}
module Main where

import Control.Monad
import Data.Monoid
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import System.Exit
import Common
import Options.Applicative

import "logic-TPTP" Codec.TPTP

data Options
  = Options
  { optPrintExport :: Bool
  }

optionsParser :: Parser Options
optionsParser = Options <$> printExportOption
  where
    printExportOption = argument auto
      $  metavar "True|False"
      <> help ("print exported result")

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optionsParser) $ mconcat
  [ fullDesc
  ]

-- Note: This test expects a list of .p files (one per line) through stdin

main ::  IO ()
main = do
  files <- lines `fmap` getContents
  --print (length files)
  Options print_export <- execParser parserInfo
  forM_ files (diff_once_twice print_export)
  exitWith ExitSuccess

diff_once_twice ::  Bool -> String -> IO ()
diff_once_twice print_export infilename'  = do
  --let tmp = "/tmp/tmp.tptp"
  putStrLn infilename'
  input <- readFile infilename'
  case findUnsupportedFormulaType input of
     Just x -> putStrLn . prettySimple . yellow . text $ ("Skipping unsupported formula type "++x)
     Nothing -> do

      let once = parse input
      let tptp = toTPTP' once
      when print_export (putStrLn $ "new tptp = " ++tptp)
      let twice = parse tptp
      let dif = mconcat (zipWith diffAFormula once twice)
      let success = (putStrLn . prettySimple . dullgreen . text $ "Ok")

      -- case dif of
      --   OtherSame -> success
      --   FormulaDiff (F Same) -> success
      --   _ -> do
      --     putStrLn . prettySimple $ dif
      --     exitWith (ExitFailure 1)

      if once==twice
         then success
         else do
           putStrLn . prettySimple $ dif
           exitWith (ExitFailure 1)
