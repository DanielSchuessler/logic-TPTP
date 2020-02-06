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

module Main where

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
import System.IO
import Control.Arrow
import Debug.Trace
import System.SimpleArgs
import Data.Generics
import Test.QuickCheck
import Data.Monoid
import Text.PrettyPrint.ANSI.Leijen
import System.Exit
import Text.Regex.PCRE.Light.Char8

import "logic-TPTP" Codec.TPTP


main = putStrLn . prettySimple . parse =<< readFile =<< getArgs
