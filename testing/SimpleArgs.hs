{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

-- Copied and modified from https://hackage.haskell.org/package/simpleargs-0.2.1
-- which is licensed under GNU LESSER GENERAL PUBLIC LICENSE Version 2.1

-- | Provide a @getArgs@ function that is slightly more advanced than the default, without
--   going the entire @System.Console.GetOpt@ route.
--
--   The idea is to return a tuple (including the 0-tuple @()@ or 1-tuple)
--   if the supplied arguments match the demands of the program (in number and in type)
--   or a sensible error message if not.
--   The returned tuple must contain elements that are in the @Typeable@ and @Read@ classes.
--
-- As an examle, here's a simple line counting program. 
-- Here @getArgs@ makes the program take a single parameter, and returns it
-- as a @String@:
--
-- > main = getArgs >>= readFile >>= print . length . lines
--
-- This program will take two parameters, a @Char@ and a @String@:
--
-- > main = do
-- >    (ch,name) <- getArgs
-- >    putStrLn (ch:"Name is: "++name)
--

module SimpleArgs (Args, getArgs) where

import qualified System.Environment as S (getArgs)
import Data.Typeable (Typeable, typeOf)

class Args a where
    -- | Return appropriately typed program arguments.
    getArgs :: IO a

argerror :: Typeable a => Int -> [String] -> a
argerror n xs = let ret = error ("Incorrect number of arguments, got "++show (length xs)++",\n"
                                ++"expected "++show n ++ " "++show (typeOf ret))
                in ret

instance Args () where
    getArgs = S.getArgs >>= return . g
        where g [] = ()
              g xs = argerror 0 xs

instance (Read b, Typeable b) => Args b where
    getArgs = S.getArgs >>= return . g
        where g [x] = myread x
              g xs = argerror 1 xs

instance (Read x, Typeable x, Read y, Typeable y) => Args (x,y) where
    getArgs = S.getArgs >>= return . g
        where g [x1,x2] = (myread x1,myread x2)
              g xs = argerror 2 xs

instance (Read t1, Typeable t1,Read t2, Typeable t2,Read t3, Typeable t3) => Args (t1,t2,t3) where
    getArgs = S.getArgs >>= return . g
        where g [x1,x2,x3] = (myread x1,myread x2,myread x3)
              g xs = argerror 3 xs

instance (Read t1,Typeable t1,Read t2,Typeable t2,Read t3,Typeable t3,Read t4,Typeable t4) => Args (t1,t2,t3,t4) where
    getArgs = S.getArgs >>= return . g
        where g [x1,x2,x3,x4] = (myread x1,myread x2,myread x3,myread x4)
              g xs = argerror 4 xs

instance (Read t1,Typeable t1,Read t2,Typeable t2,Read t3,Typeable t3,Read t4,Typeable t4,Read t5,Typeable t5) => Args (t1,t2,t3,t4,t5) where
    getArgs = S.getArgs >>= return . g
        where g [x1,x2,x3,x4,x5] = (myread x1,myread x2,myread x3,myread x4,myread x5)
              g xs = argerror 5 xs

-- | Attempt to parse the parameter as various types
myread :: (Typeable a, Read a) => String -> a
myread s = let ret = case map reads [s,sq s,dq s,lq s] of
                       ([(x,"")]:_) -> x
                       (_:[(c,"")]:_) -> c
                       (_:_:[(str,"")]:_) -> str
                       (_:_:_:[(l,"")]:_) -> l
                       _ -> error ("Couldn't parse parameter "++show s++" as type "++show (typeOf ret)++".")
           in ret
    where
      -- different types of quoting
      sq x = "'"++x++"'"
      dq x = "\""++x++"\""
      lq x = "["++x++"]"
