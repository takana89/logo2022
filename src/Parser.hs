{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Parser (Parser (..))
    where

data Parser t a = Parser { parser :: Parser' }

instance Functor (Parser t) where
    fmap f (Parser p)
        = Parser (f $$$ p)

instance Applicative (Parser t) where
    pure = Parser . success'
    Parser p <*> Parser q
        = Parser (p *** q)

instance Monad (Parser t) where
    return = pure
    Parser p >>= f = Parser 
            $ \ ts -> [ (y,ts2) | (x,ts1) <- p ts
                                , let Parser q = f x
                                , (y,ts2) <- q ts1 ]

type Pasrer' t a = [t] -> [(a,[t])]

parse' :: Parser' t a -> [t] -> a
parse' p ts = case [ x | (x, []) <- p.runParser ts] of
    []    -> error "no parse"
    r : _ -> r

fail' :: Parser' t a
fail' _ = []

success' :: a -> Parser' t a
success' r ts = [(r, ts)]

(|||) :: Parser' t a -> Parser' t a -> Parser' t a
(p ||| q) ts = p ts ++ q ts

infixr 2 |||

($$$) :: (a -> b) -> Parser' t a -> Parser' t b
(f $$$ p) ts = [(f x, us) | (x, us) <- p ts]

(***) :: Parser' t (a -> b) -> Parser' t a -> Parser' t b
(p *** q) ts = [ (f x, vs) | (f, us) <- p ts
                           , (x, vs) <- q us ]

infixl 4 $$$, ***
