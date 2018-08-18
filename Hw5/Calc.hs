{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Calc where

import qualified Data.Map as M
import ExprT
import Parser
import StackVM

eval :: ExprT -> Integer
eval (Lit n) = n
eval (ExprT.Add a b) = eval a + eval b
eval (ExprT.Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr s = case parseExp ExprT.Lit ExprT.Add ExprT.Mul s of
  Just expr -> Just (eval expr)
  Nothing   -> Nothing

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = ExprT.Add
  mul = ExprT.Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = lit (max a b)
  mul (MinMax a) (MinMax b) = lit (min a b)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit n = Mod7 (n `mod` 7)
  add (Mod7 a) (Mod7 b) = lit (a + b)
  mul (Mod7 a) (Mod7 b) = lit (a * b)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

instance Expr Program where
  lit n = [PushI n]
  add a b = StackVM.Add : a ++ b
  mul a b = StackVM.Mul : a ++ b

compile :: String -> Maybe Program
compile = parseExp lit add mul

class HasVar a where
  var :: String -> a

data VarExprT = VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              | Var String
  deriving (Show, Eq)
instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul
instance HasVar VarExprT where
  var = Var

instance HasVar (M.Map String Integer -> Maybe Integer) where
  var = M.lookup
instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = const . Just
  add a b m = fmap (+) (a m) <*> b m
  mul a b m = fmap (*) (a m) <*> b m

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs expr = expr $ M.fromList vs
