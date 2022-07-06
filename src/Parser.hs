module Parser
    where

type Parser t a = [t] -> [(a, [t])]

parse :: Parser t a -> [t] -> a
parse p ts = case [ x | (x, []) <- p ts] of
    []    -> error "no parse"
    r : _ -> r

fail :: Parser t a
fail _ = []

success :: a -> Parser t a
success r ts = [(r, ts)]

(|||) :: Parser t a -> Parser t a -> Parser t a
(p ||| q) ts = p ts ++ q ts

infixr 2 |||

($$$) :: (a -> b) -> Parser t a -> Parser t b
(f $$$ p) ts = [(f x, us) | (x, us) <- p ts]

(***) :: Parser t (a -> b) -> Parser t a -> Parser t b
(p *** q) ts = [ (f x, vs) | (f, us) <- p ts
                           , (x, vs) <- q us ]

infixl 4 $$$, ***
