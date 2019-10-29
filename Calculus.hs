module Calculus where

import Data.Maybe

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)
-- UNARY OPERATOR -- takes ONE argument
data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)
-- BINARY OPERATOR -- takes TWO arguments
data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]

---------------------------------------------------------------------------
-- Type classes and class instances

class Vars a where
  x, y, z :: a

instance Vars Exp where
  x = Id "x"
  y = Id "y"
  z = Id "z"

instance Vars Double where
  x = 4.3
  y = 9.2
  z = -1.7

instance Num Exp where
  fromInteger = undefined
  negate      = undefined
  (+)         = undefined
  (*)         = undefined
-- Leave the following two undefined...
  signum      = undefined
  abs         = undefined

instance Fractional Exp where
  fromRational = undefined
  (/)          = undefined
-- Leave the following one undefined...
  recip        = undefined

instance Floating Exp where
  sin     = undefined
  cos     = undefined
  log     = undefined
-- Leave the following fifteen undefined...
  tan     = undefined
  asin    = undefined
  acos    = undefined
  atan    = undefined
  pi      = undefined
  exp     = undefined
  sqrt    = undefined
  (**)    = undefined
  logBase = undefined
  sinh    = undefined
  cosh    = undefined
  tanh    = undefined
  asinh   = undefined
  acosh   = undefined
  atanh   = undefined

---------------------------------------------------------------------------
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp keySought envir = fromJust (lookup keySought envir)
----------------------------------------------------------------------------
showExp :: Exp -> String
showExp ( Val v ) = show v 
showExp ( Id str ) = str
showExp ( UnApp op exp) = "(" ++ lookUp op dictionaryUn ++ showExp exp ++ ")"
  where
    dictionaryUn = [(Neg, "-"), (Sin, "sin") , (Cos, "cos"), (Log, "log")]
showExp (BinApp bi x1 x2) = "(" ++ showExp x1 ++ lookUp bi dictionaryBin ++ showExp x2 ++ ")"
  where
    dictionaryBin = [(Add, "+" ), (Mul, "*" ), (Div, "/" )]

------------------------------------------------------------------------------
eval :: Exp -> Env -> Double
eval (Val v) _ = v
eval (Id search) envir = lookUp search envir
eval (UnApp op ex) envir = lookUp op dictUn (eval ex envir)
    where
      dictUn = [( Neg, ((-1) * )) , (Sin, sin ) , (Cos, cos ), (Log, log) ]
eval (BinApp bi x1 x2) envir = lookUp bi dictBin (eval x1 envir) (eval x2 envir)
  where
    dictBin = [(Add, (+)), (Mul, (*)), (Div, (/))]

--------------------------------------------------------------

diff :: Exp -> String -> Exp
diff (Val v) _  = Val 0 -- rules for differentiating constants
diff (Id x) wrt 
    | x == wrt  = Val 1
    | otherwise = Val 0
--rules for differentiating expressions when not in terms of the 
--correct constant; treats it as if it were a constant.    
diff (UnApp op expr) wrt
    | op == Neg   = UnApp Neg (diff expr wrt)
    | op == Sin   =  BinApp Mul (UnApp Cos expr) (diff expr wrt)
    | op == Cos   = UnApp Neg (BinApp Mul (UnApp Sin expr) (diff expr wrt))
    | op == Log   = BinApp Div (diff expr wrt) (expr) 
diff (BinApp bi x1 x2) wrt
    | bi == Add = BinApp Add (diff x1 wrt)  (diff x2 wrt)
    | bi == Mul = BinApp Add product2 product1
    | otherwise = BinApp Div (quotient1) (quotient2)
      where
        quotient1 = BinApp Add (BinApp Mul x1 (diff x2 wrt)) (UnApp Neg (BinApp Mul  (diff x1 wrt ) x2 ))
        quotient2 = BinApp Mul x2 x2
        product1 = BinApp Mul (diff x1 wrt) (x2)
        product2 = BinApp Mul (x1) (diff x2 wrt)
-----------------------------------------------------------------
maclaurin :: Exp -> Double -> Int -> Double
maclaurin expr x n 
  = sum ( zipWith3 (\a b c -> (a*b) / c ) evaluated powersOfX factorials)
    where
      factorials     = scanl (*) 1 [1..]
-- scanl recursively applies function to compute factorials of numbers; 
-- no need to take n values.
      functionValues = take n (iterate( flip diff "x") expr)
      powersOfX      = iterate  (x * ) 1
      evaluated      = map (flip eval  [("x", 0)]) functionValues

---------------------------------------------------------------------------
-- Test cases...

e1, e2, e3, e4, e5, e6 :: Exp

-- 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- x*x+y-7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

-- log(3*x^2+2)::Exp
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
                           (Val 2.0))
