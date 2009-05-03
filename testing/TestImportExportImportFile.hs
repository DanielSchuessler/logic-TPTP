{-# OPTIONS 
 -fglasgow-exts 
 -XCPP 
 -XTemplateHaskell 
 -XNamedFieldPuns 
 -XRecordWildCards 
 -XDeriveDataTypeable 
 -XOverlappingInstances
 -XPackageImports
 -fwarn-incomplete-patterns
 #-}

module TestImportExportImportFile where

import Control.Monad
import Control.Monad.State
import Control.Applicative((<$>),(<*>))
import Control.Arrow
import Text.Printf.TH
import Data.Maybe
import Data.List as L
import Data.Map as M
import Data.Set as S
import qualified Data.ByteString as B
import Data.Function
import System.Process
import System.UTF8IO
import Control.Arrow
import Debug.Trace
import Prelude()
import UTF8Prelude hiding(catch)
import System.SimpleArgs
import Data.Generics
import Test.QuickCheck
import Data.Monoid
import Text.PrettyPrint.ANSI.Leijen
import System.Exit
import Text.Regex.PCRE.Light.Char8
import Common
    
import "logic-TPTP" Codec.TPTP


main = do
  files <- lines `fmap` getContents
  --print (length files)
  print_export <- getArgs
  forM_ files (diff_once_twice print_export)
  exitWith ExitSuccess
         
diff_once_twice print_export infilename'  = do
  --let tmp = "/tmp/tmp.tptp"
  putStrLn infilename'
  input <- readFile infilename'
  if (isThf input) 
     then putStrLn . prettySimple . yellow . text $ "Skipping Thf"
     else do

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



