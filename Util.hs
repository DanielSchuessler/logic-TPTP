{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
module Util where

import Data.Typeable

packageName :: String
packageName = "logic-TPTP"

-- | Gets the TypeRep of @c@ (not of @c ()@)
getTyRep1 :: forall c. Typeable1 c => c () -> TypeRep
getTyRep1 _ = mkTyConApp (typeRepTyCon (typeOf1 (undefined :: c ()))) []


mkTypeOfForRank2Kind :: forall x c. Typeable1 c => String -> String -> x c -> TypeRep
mkTypeOfForRank2Kind moduleName typeName = const tr
    where
        tc = mkTyCon3 packageName moduleName typeName  
        tr = mkTyConApp tc [getTyRep1 (undefined :: c ())]

