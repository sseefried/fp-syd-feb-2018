{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module TypedInterp where

import Data.Proxy

data Value a where
  VBoolean :: Bool      -> Value Bool
  VInt     :: Int       -> Value Int
  VFloat   :: Float     -> Value Float
  VString  :: String    -> Value String
  VArray   :: [Value a] -> Value [a]

deriving instance Show (Value a)


data Exp a where
  Plus   :: Num a => Exp a -> Exp a -> Exp a
  Value  :: Value a -> Exp a

eval (Plus a b) = eval a + eval b
eval (Value v)  = v

--------------------------------------------------------------------------------

instance Num (Value Int) where
  (VInt n) + (VInt m) = VInt (n + m)
  (*)         = error "undefined"
  signum      = error "undefined"
  fromInteger = error "undefined"
  negate      = error "undefined"
  abs         = error "undefined"

instance Num (Value String) where
  (VString s) + (VString s') = VString (s ++ s')
  (*)         = error "undefined"
  signum      = error "undefined"
  fromInteger = error "undefined"
  negate      = error "undefined"
  abs         = error "undefined"

instance Num (Value Float) where
  VFloat n + VFloat n' = VFloat (n + n')
  (*)         = error "undefined"
  signum      = error "undefined"
  fromInteger = error "undefined"
  negate      = error "undefined"
  abs         = error "undefined"

--------------------------------------------------------------------------------
ins :: Value a -> Value [a] -> Value [a]
ins v vas =
  case vas of
  	VArray vs -> VArray (v:vs)
  	_ -> error "not possible"