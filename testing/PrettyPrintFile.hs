{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Main where

import Control.Monad
import Control.Monad.State
import Control.Applicative((<$>),(<*>))
import Control.Arrow
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
import Data.Generics
import Test.QuickCheck
import Data.Monoid
import System.Exit
import Text.Regex.PCRE.Light.Char8

import Codec.TPTP
import SimpleArgs

main = putStrLn . prettySimple . parse =<< readFile =<< getArgs
