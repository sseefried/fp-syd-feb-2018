{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.Char (isNumber)
import Data.List (intercalate, sort)
import Text.Read (readMaybe)
import Debug.Trace

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
         | UnaryPlus Exp
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
        Just f  -> f o vs
        Nothing -> error ("could not find method " ++ nm)
    _        -> VUndefined -- can only method call on objects
eval (Plus e1 e2) =
  let lref  = eval e1
      lval  = lref -- since we don't have references
      rref  = eval e2
      rval  = rref -- since we don't have references
      lprim = ecmaToPrimitive Nothing lval
      rprim = ecmaToPrimitive Nothing rval
  in
    case True of
      _ | typeOf lprim == TString || typeOf rprim == TString
        -> let lstr = ecmaToString lprim
               rstr = ecmaToString rprim
           in VString (lstr ++ rstr)

      _ -> let lnum = ecmaToNumber(lprim)
               rnum = ecmaToNumber(rprim)
           in plusNumber lnum rnum
eval (UnaryPlus e) = VNumber $ ecmaToNumber (eval e)

lookupMethod :: String -> Object -> Maybe MethodFun
lookupMethod nm o =
  case o of
    Object _ -> lookup nm objectMethods
    Class _ methods parent ->
      case lookup nm methods of
        Just method -> Just method
        Nothing     -> lookupMethod nm parent


-- default methods of Object
objectMethods :: [(String, MethodFun)]
objectMethods = [ ("toString", \_ _ -> VString "[object Object]")
                , ("valueOf", \o _ -> VObject o) ]

type Hint = String

ecmaToPrimitive :: Maybe Hint -> Value -> Value
ecmaToPrimitive mbType = \case
  VObject obj ->
    let hint = case mbType of
                 Nothing -> "default"
                 Just h  -> h
        mbExoticToPrim = lookupMethod "toPrimitive" obj
    in case mbExoticToPrim of
         Just f ->
           let result = f obj [VString hint]
           in case typeOf result of
                TObject -> error "TypeError"
                _       -> result
         Nothing ->
           let hint' = case hint of
                         "default" -> "number"
                         h    -> h
           in ordinaryToPrimitive obj hint'
  prim        -> prim

-- can only be called on objects
ordinaryToPrimitive :: Object -> Hint -> Value
ordinaryToPrimitive o hint =
  let methodNames =
       case hint of
         "string" -> [ "toString", "valueOf"  ]
         "number" -> [ "valueOf",  "toString" ]
         _ -> error $ "hint should be 'number' or 'string' but was \"" ++
                      hint ++ "\""
  in callFirstFoundMethod methodNames
  where
    callFirstFoundMethod [] = error "TypeError"
    callFirstFoundMethod (m:ms) =
          case lookupMethod m o of
            Just f  ->
              case f o [] of
                VObject o -> callFirstFoundMethod ms
                -- ^ result was an object (not primitive). Keep going
                result -> result
            Nothing -> callFirstFoundMethod ms


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
ecmaToNumber = \case
  VUndefined -> nan
  VNull      -> 0
  VBoolean b -> if b then 1 else 0
  VNumber n  -> n
  VString s  ->
    if s == "" then 0
    else case readMaybe s of
           Just n  -> n
           Nothing -> nan
  o@(VObject _)  -> ecmaToNumber $ ecmaToPrimitive (Just "number") o

-- ToString
ecmaToString :: Value -> String
ecmaToString = \case
  VUndefined -> "undefined"
  VNull      -> "null"
  VBoolean b -> if b then "true" else "false"
  VNumber n  -> show n
  VString s  -> s
  o@(VObject _)  -> ecmaToString $ ecmaToPrimitive (Just "string") o


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

emptyArrayExp :: Exp
emptyArrayExp = Value $ VObject emptyArray

emptyObjectExp = Value $ VObject $ Object []

-- [] + [] => ""
wat1 = eval $ Plus emptyArrayExp emptyArrayExp
-- [] + {}  => 0
wat2 = eval $ Plus emptyArrayExp emptyObjectExp

-- {} + [] => 0
wat3 = eval $ UnaryPlus emptyArrayExp

-- {} + {} => NaN
wat4 = eval $ UnaryPlus emptyObjectExp

-- ({} + {}) => "[object Object][object Object]"
wat3' = eval $ Plus emptyObjectExp emptyObjectExp

-- ({} + []) =>
wat4' = eval $ Plus emptyObjectExp emptyArrayExp

