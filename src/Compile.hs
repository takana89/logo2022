module Compile where

import Syntax
import World

type ProcEnv = [(String, Int)]
type ValEnv = [(String,Value)]

valExpr :: ValEnv -> Term -> Value
valExpr env (ENum n) = Float n
valExpr env (EBool b) = Bool b
valExpr env (EVar v) = case lookup v env of
    Just val -> val
    Nothing -> error "valExpr: unbound var"
valExpr env (EArith e1 Add e2) = case (valExpr env e1 , valExpr env e2) of
    (Float v1,Float v2) -> Float (v1+v2)
valExpr env (EArith e1 Sub e2) = case (valExpr env e1 , valExpr env e2) of
    (Float v1,Float v2) -> Float (v1-v2)
valExpr env (EArith e1 Mul e2) = case (valExpr env e1 , valExpr env e2) of
    (Float v1,Float v2) -> Float (v1*v2)
valExpr env (EArith e1 Div e2) = case (valExpr env e1 , valExpr env e2) of
    (Float v1,Float v2) -> Float (v1/v2)
valExpr env (ECompare e1 Lt e2) = case (valExpr env e1 , valExpr env e2) of
    (Float v1,Float v2) -> Bool (v1<v2)
valExpr env (ECompare e1 Gt e2) = case (valExpr env e1 , valExpr env e2) of
    (Float v1,Float v2) -> Bool (v1>v2)

valExpr env _ = error "valExpr: not arithmetic or comparing"

evalTerm :: ValEnv -> ProcEnv -> Term -> Instruction
evalTerm venv penv (EProcCall proc args) = case length args of
    0 -> maybe (error $ "evalExpr: unknown " ++ proc) id (lookup proc proc0s)
    1 -> case valExpr venv $ head args of
        Float x -> maybe (error $ "evalExpr: unknown " ++ proc) ($ x) (lookup proc proc1s)
    2 -> case map (valExpr venv) args of
        [Float x,Float y] -> maybe (error $ "evalExpr: unknown " ++ proc) (($ y) . ($ x)) (lookup proc proc2s)
     
    