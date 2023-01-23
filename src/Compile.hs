{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Compile where

import Control.Arrow
import Data.List
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

evalExpr :: Expr -> World -> (World, [World])
evalExpr e w = case e of
    ESeq es -> second concat $ mapAccumL phi w es
        where
            phi wd ex = evalExpr ex wd
    EIf c t f -> case valExpr w.valenv c of
        Bool True -> evalExpr t w
        Bool False -> evalExpr f w
    EApp proc args -> apply proc args w
    _ -> (w', [w'])
        where
            w' = w { acc = valExpr w.valenv e, pictflag = False }

apply :: String -> [Expr] -> World -> (World, [World])
apply f args w = case lookup f builtins of
    Just (a, g) -> w'
        where
            w' = g (take a args'') w
    Nothing -> case lookup f w.procenv of
        Nothing -> error $ "apply: unknown procedure: " ++ f
        Just (params, e) -> case zip params args' of
            env' -> evalExpr e (w { valenv = env' ++ w.valenv })
    where
        args' = map (valExpr w.valenv) args
        args'' = map fromVal args'
        fromVal (Float x) = x
        fromVal (Bool b) = fromIntegral (fromEnum b)

        