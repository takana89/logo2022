{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Compile where

import Syntax
import World


valExpr :: ValEnv -> Expr -> Value
valExpr env (ENum n) = Float n
valExpr env (EBool b) = Bool b
valExpr env (EVar v) = case lookup v env of
    Just val -> val
    Nothing -> error "valExpr: unbound var"
valExpr env (EPlus e1 e2) = case (valExpr env e1 , valExpr env e2) of
    (Float v1,Float v2) -> Float (v1+v2)
valExpr env (EMinus e1 e2) = case (valExpr env e1 , valExpr env e2) of
    (Float v1,Float v2) -> Float (v1-v2)
valExpr env (EMul e1 e2) = case (valExpr env e1 , valExpr env e2) of
    (Float v1,Float v2) -> Float (v1*v2)
valExpr env (EDiv e1 e2) = case (valExpr env e1 , valExpr env e2) of
    (Float v1,Float v2) -> Float (v1/v2)
valExpr env (ELess e1 e2) = case (valExpr env e1 , valExpr env e2) of
    (Float v1,Float v2) -> Bool (v1<v2)
valExpr env (EGreater e1 e2) = case (valExpr env e1 , valExpr env e2) of
    (Float v1,Float v2) -> Bool (v1>v2)

valExpr env _ = error "valExpr: not arithmetic or comparing"

evalExpr :: Expr -> Instruction
evalExpr (EApp proc args) w = case lookup proc builtins of
    Just (a, f) -> apply f (take a args') w
    Nothing -> case lookup proc w.procenv of
        Nothing -> error $ "evalExpr: unknown procedure: " ++ proc
        Just (params, e) -> case zip params args' of
            env' -> evalExpr e (w { valenv = env' ++ w.valenv })
    where
        args' = map (valExpr w.valenv) args

apply :: Procedure0 -> [Value] -> Instruction
apply f args = f (map untag args)
    where
        untag (Float x) = x
        