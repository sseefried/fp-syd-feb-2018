{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module JSInterp where

import Data.Char (isNumber)
import Data.List (intercalate, sort)

data Type = TUndefined
	      | TNull
          | TBoolean
          | TNumber
--          | TSymbol
          | TString
          | TObject
          deriving (Eq, Show)

data Value = VUndefined
           | VNull
           | VBoolean Bool
           | VNumber Double
--           | VSymbol String
           | VString String
           | VObject Object
           deriving (Show, Ord)


data Object = Object [(String, Value)]
            | Class
                 String -- name
                 [ Method ] -- methods
                 Object -- parent
            deriving (Show, Ord)

type MethodFun = Object -> [Value] -> Value

type Method = (String, MethodFun)

data Exp = Plus Exp Exp
         | Value Value
         | MethodCall Exp [Exp] String
         deriving (Show)

instance Eq Value where
    VUndefined == VUndefined  = True
    VNull      == VNull       = True
    VBoolean b == VBoolean b' = b == b'
    VNumber n == VNumber n'   = n == n'
    VString s == VString s'   = s == s'
    VObject _ == VObject _    = False

instance Show MethodFun where
    show _ = "<method>"

instance Ord MethodFun where
    compare _ _ = LT

instance Eq MethodFun where
    _ == _ = False

instance Eq Object where
    _ == _ = False

{-

  I'm going to leave out environments and variables for the moment.

-}

eval :: Exp -> Value
eval (Value v) = v
eval (MethodCall e es nm) =
  case (eval e, map eval es) of
    (VObject o, vs) ->
      case lookupMethod nm o of
        Just f -> f o vs
        Nothing -> error ("could not find method " ++ nm)
    _        -> VUndefined -- can only method call on objects
eval (Plus e1 e2) =
  let lref = eval e1
      lval = lref -- since we don't have references
      rref = eval e2
      rval = rref -- since we don't have references
      lprim = toPrimitive lval
      rprim = toPrimitive rval
  in
    case True of
      _ | typeOf(lprim) == TString || typeOf(rprim) == TString
        -> let lstr = ecmaToString lprim
               rstr = ecmaToString rprim
           in VString (lstr ++ rstr)

      _ -> let lnum = ecmaToNumber(lprim)
               rnum = ecmaToNumber(rprim)
           in plusNumber lnum rnum

lookupMethod :: String -> Object -> Maybe MethodFun
lookupMethod nm o =
  case o of
    Object props -> Nothing -- FIXME: Built-in functions required!
    Class _ methods parent ->
      case lookup nm methods of
        Just method -> Just method
        Nothing     -> lookupMethod nm parent



toPrimitive :: Value -> Value
toPrimitive = \case
  VObject obj -> undefined
  prim        -> prim

typeOf :: Value -> Type
typeOf = \case
  VUndefined -> TUndefined
  VNull      -> TNull
  VBoolean _ -> TBoolean
  VNumber _  -> TNumber
  VString _  -> TString
  VObject _  -> TObject

-- ToNumber
ecmaToNumber :: Value -> Double
ecmaToNumber = error "toNumber undefined"

-- ToString
ecmaToString :: Value -> String
ecmaToString = error "toString undefined"

-- built-in addition. Only works on numbers
plusNumber :: Double -> Double -> Value
plusNumber d1 d2 = VNumber (d1 + d2)

-- Hacky way to get NaN
nan :: Double
nan = sqrt (-1)

--------------------------------------------------------------------------------
-- Arrays

emptyArray :: Object
emptyArray = Class "Array" [ toStringMethod ] (Object [])
  where
    toStringMethod =  ("toString", toStr)
    toStr o _ = VString (intercalate "," (map (pretty . snd) $ sort $ objectProperties o))
    arrayLength :: Object -> Int
    arrayLength o = length $ numericProperties o

objectProperties :: Object -> [(String, Value)]
objectProperties (Object props) = props
objectProperties (Class _ _ parent) = objectProperties parent

-- Returns the name of all the numeric keys
numericProperties :: Object -> [(String, Value)]
numericProperties o =
  filter (isNumberString . fst) (objectProperties o)

--------------------------------------------------------------------------------
-- Pretty printining

pretty :: Value -> String
pretty = \case
  VUndefined  -> "undefined"
  VNull       -> "null"
  VNumber  n  -> show n
  VBoolean b  -> if b then "true" else "false"
  VString  s  -> show s
  VObject  _  -> "[object Object]"

--------------------------------------------------------------------------------
-- Helpers

isNumberString :: String -> Bool
isNumberString = all isNumber


--------------------------------------------------------------------------------
-- Tests

t1 = eval (MethodCall (Value (VObject emptyArray)) [] "toString")