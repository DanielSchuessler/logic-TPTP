----------------------------------------------------------------------
-- |
-- Copyright   :  (c) Daniel Schüssler 2009
-- License     :  GPL v3
-- 
-- Maintainer  :  daniels@community.haskell.org
-- Stability   :  Experimental
-- Portability :  Requires language extensions
-- 
-- Lib for generating TPTP 3
----------------------------------------------------------------------



{-# OPTIONS_GHC 
 -fglasgow-exts 
 -XCPP 
 -XTemplateHaskell 
 -XNamedFieldPuns 
 -XRecordWildCards 
 -XDeriveDataTypeable 
 -XNoMonomorphismRestriction
 -XTypeFamilies
 -fno-warn-missing-signatures
 -XOverlappingInstances
 -XNoImplicitPrelude
 -XOverloadedStrings
 #-}


module Codec.TPTP
                   where

import Text.Printf.TH
import Control.Monad
import Data.Char
import Prelude hiding((||))
import Data.Data
import Data.List
import Data.String
import Debug.Trace

    
#include "../macros.h"
         
newtype Var = Var String da
         
data Prop = QProp Quantifier [Var] Prop
          | BinOpProp BinOp Prop Prop
          | Eql Term Term
          | Not Prop
          | Labeled String Prop
          | AppPred PredSym [Term]
               da

data PredSym = PredSym Int String
             da
               
data Quantifier = All | Exist da
                
data BinOp = And | Or | Implies | Implied | Equiv da

data Term = VarTerm Var 
          | AppFun FunSym [Term] da
            
data FunSym = FunSym Int String
            da
            


constant = FunSym 0
(&) = BinOpProp And
(||) = BinOpProp Or
(-->) = BinOpProp Implies
(<--) = BinOpProp Implied
(<->) = BinOpProp Equiv
(===) = Eql
neg = Not 
      
infixr 3 &
infixr 2 ||
infixr 1 -->
infixl 1 <--
infix 1 <->
infix 5 ===


class ToLOP a where
    toLOP :: a -> String

instance ToLOP Var where
    toLOP (Var x) | isUpper (head x) = x
    toLOP (Var x) = error "variable names must be uppercase in LOP"
                    
                    
instance ToLOP Term where
    toLOP (AppFun (FunSym ar str) terms) | ar == length terms = 
           
      $(printf "%s(%s)") str (concat . intersperse ", " . fmap toLOP $ terms) 


    toLOP (VarTerm x) = toLOP x

instance ToLOP Prop where

    toLOP QProp q vs p = case q of
                           All -> trace "skipping universal quantifier; they're implicit in LOP"
                                 (toLOP p)
                           Exist -> error "existential quantifiers not supported in LOP (TODO: skolemization)"
                           
    toLOP BinOpProp op p1 p2 = $(printf "%s(%s, %s)") (toLOP op) (toLOP p1) (toLOP p2)
    toLOP Eql Term Term
    toLOP Not Prop
    toLOP Labeled String Prop
    toLOP AppPred PredSym [Term]
  
          instance ToLOP
