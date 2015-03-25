{-# OPTIONS -Wall #-}
{-# LANGUAGE PackageImports #-}
module TestImportExportImportFile where

import Control.Monad
import Data.Maybe
import Data.List as L
import Data.Function
import System.IO
import System.SimpleArgs
import Data.Monoid
import Text.PrettyPrint.ANSI.Leijen
import System.Exit
import Common

import "logic-TPTP" Codec.TPTP


-- Note: This test expects a list of .p files (one per line) through stdin

main ::  IO ()
main = do
  files <- lines `fmap` getContents
  --print (length files)
  print_export <- getArgs
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
