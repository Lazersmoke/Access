{-# LANGUAGE TypeOperators #-}
module Data.Access where

import Control.Category
import Prelude hiding (id,(.))

data Access w p = Access (w -> p) ((p -> p) -> w -> w)
-- Access laws:
--
-- A: grab a . set a b $ x == b
-- B: grab a . lift a f . set a b $ x == f b
--
-- A: If you set something to a value, then get that something, it is that value
-- B: If you set a value, then lift a function on it, then grab it, it should be the same as applying that function to the initial value

instance Category Access where
  id = whole
  --(.) :: (b ~> c) -> (a ~> b) -> (a ~> c)
  (Access g2 l2) . (Access g1 l1) = Access (g2 . g1) (l1 . l2)

type w ~> p = Access w p

grab,(~>>) :: w ~> p -> w -> p
grab (Access g _) = g
(~>>) = grab

(<<~) :: w -> w ~> p -> p
(<<~) = flip grab

lift,(>&>) :: w ~> p -> (p -> p) -> w -> w
lift (Access _ l) = l
(>&>) = lift
infixr 6 >&>

liftMap :: Functor f => w ~> f p -> (p -> p) -> w -> w
liftMap a f = a >&> fmap f

set,(>@>) :: w ~> p -> p -> w -> w
set a n = a >&> const n
(>@>) = set

-- Accessors for lists, tuples, etc
whole :: a ~> a
whole = Access id id

swizzle :: (a ~> b) -> (a ~> b) -> a -> a
swizzle fromA toA input = toA >@> (fromA ~>> input) $ input
-- swizzle = flip flip id . (ap .) . flip ((.) . set) . grab
-- why not

-- UNSAFE AF WARNING!
headA :: [a] ~> a
headA = Access head (\f (x:xs) -> f x : xs)

tailA :: [a] ~> [a]
tailA = Access tail (\f (x:xs) -> x : f xs)

pairFirst :: (a,b) ~> a
pairFirst = Access fst (\f (x,y) -> (f x,y))

pairSecond :: (a,b) ~> b
pairSecond = Access snd (\f (x,y) -> (x,f y))

tripleFirst :: (a,b,c) ~> a
tripleFirst = Access (\(a,_,_) -> a) (\f (a,b,c) -> (f a,b,c))

tripleSecond :: (a,b,c) ~> b
tripleSecond = Access (\(_,b,_) -> b) (\f (a,b,c) -> (a,f b,c))

tripleThrid :: (a,b,c) ~> c
tripleThrid = Access (\(_,_,c) -> c) (\f (a,b,c) -> (a,b,f c))

{-
Example of how to set up your own datatype to work with accessors:

data Kitten = Kit {
  _age :: Integer,
  _name :: String
  } deriving Show

-- You can probably Template Haskell away the rest, but here it is anyway:
-- Beacuse I'm bad at TH :(

kittenAge :: Kitten ~> Integer
kittenAge = Access _age (\f k -> k {_age = f (_age k)})

kittenName :: Kitten ~> String
kittenName = Access _name (\f k -> k {_name = f (_name k)})
-}
