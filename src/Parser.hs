{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}
module Parser
    ( Parser (..)
    , failure
    , success
    , (+++)
    , satisfy
    , token
    , tokens
    , munch
    , munch1
    , many
    , many1
    , between
    , chainl
    , chainl1
    , option
    , pair
    , parse
    )
    where

import Data.Bool
import Data.List

data Parser t a = Parser { parser :: Parser' t a }

failure :: Parser t a
failure = Parser (const [])

success :: a -> Parser t a
success = pure

infixr 2 +++
infixl 2 <++

(+++) :: Parser t a -> Parser t a -> Parser t a
Parser p +++ Parser q = Parser (p ||| q)

(<++) :: Parser t a -> Parser t a -> Parser t a
Parser p <++ Parser q = Parser (p <|| q)

satisfy :: (t -> Bool) -> Parser t t
satisfy p = Parser
          $ \ case
            t:ts | p t -> [(t, ts)]
            _          -> []

token :: Eq t => t -> Parser t t
token tok = satisfy (tok ==)

tokens :: Eq t => [t] -> Parser t [t]
tokens toks = Parser { parser = parser }
    where
        parser ts 
            = bool [] [(toks, drop (length toks) ts)] (toks `isPrefixOf` ts)

munch :: (t -> Bool) -> Parser t [t]
munch p = Parser { parser = parser }
    where
        parser ts 
            = case span p ts of
                (us,vs) -> [(us,vs)]

munch1 :: (t -> Bool) -> Parser t [t]
munch1 cond = (:) <$> satisfy cond <*> munch cond

many, many1 :: Parser t a -> Parser t [a]
many p = many1 p <++ success []
many1 p = (:) <$> p <*> many p

between :: Parser t a -> Parser t b -> Parser t c -> Parser t c
between op cl p = op *> p <* cl

chainl :: Parser t a -> Parser t (a -> a -> a) -> a -> Parser t a
chainl p op x = chainl1 p op +++ success x

chainl1 :: Parser t a -> Parser t (a -> a -> a) -> Parser t a
chainl1 p op = scan
    where
        scan   = p >>= rest
        rest x = do
            { f <- op
            ; y <- scan 
            ; return (f x y)
            } +++ return x

option :: a -> Parser t a -> Parser t a
option x p = p +++ return x

pair :: Parser t a -> Parser t b -> Parser t (a,b)
pair p q = (,) <$> p <*> q

---

instance Functor (Parser t) where
    fmap f (Parser p)
        = Parser (f $$$ p)

instance Applicative (Parser t) where
    pure = Parser . _success
    Parser p <*> Parser q
        = Parser (p *** q)

instance Monad (Parser t) where
    return = pure
    Parser p >>= f = Parser 
            $ \ ts -> [ (y,ts2) | (x,ts1) <- p ts
                                , let Parser q = f x
                                , (y,ts2) <- q ts1 ]

-- 

type Parser' t a = [t] -> [(a,[t])]

_parse :: Parser' t a -> [t] -> a
_parse p ts = case [ x | (x, []) <- p ts] of
    []    -> error "no parse"
    r : _ -> r

_fail :: Parser' t a
_fail _ = []

_success :: a -> Parser' t a
_success r ts = [(r, ts)]

(|||) :: Parser' t a -> Parser' t a -> Parser' t a
(p ||| q) ts = p ts ++ q ts

(<||) :: Parser' t a -> Parser' t a -> Parser' t a
(p <|| q) ts = p ts <+ q ts

(<+) :: [a] -> [a] -> [a]
[] <+ ys = ys
xs <+ _  = xs

infixr 2 |||
infixl 2 <||

($$$) :: (a -> b) -> Parser' t a -> Parser' t b
(f $$$ p) ts = [(f x, us) | (x, us) <- p ts]

(***) :: Parser' t (a -> b) -> Parser' t a -> Parser' t b
(p *** q) ts = [ (f x, vs) | (f, us) <- p ts
                           , (x, vs) <- q us ]

infixl 4 $$$, ***

parse :: Parser t a -> [t] -> a
parse p ts = case [ a | (a,[]) <- p.parser ts ] of
    []  -> error "parse: no parse"
    r:_ -> r
