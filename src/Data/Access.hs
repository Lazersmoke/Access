{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Access where

class Grab w a where
  type PartGrab w a :: *
  grab :: a -> w -> PartGrab w a

class Lift w a where
  type PartLift w a :: *
  lift :: a -> (PartLift w a -> PartLift w a) -> w -> w


-- Access laws:
--
-- A: grab a . set a b $ x == b
-- B: grab a . lift a f . set a b $ x == f b
--
-- A: If you set something to a value, then get that something, it is that value
-- B: If you set a value, then lift a function on it, then grab it, it should be the same as applying that function to the initial value
{-
instance Category Access where
  id = whole
  --(.) :: (b ~> c) -> (a ~> b) -> (a ~> c)
  (Access g2 l2) . (Access g1 l1) = Access (g2 . g1) (l1 . l2)
-}
(~>>) :: (Grab w a, PartGrab w a ~ p) => a -> w -> p
(~>>) = grab

(<<~) :: (Grab w a, PartGrab w a ~ p) => w -> a -> p
(<<~) = flip grab

(>&>) :: (Lift w a, PartLift w a ~ p) => a -> (p -> p) -> w -> w
(>&>) = lift
infixr 6 >&>


-- w is ([a],[b]) a is One p is a
liftMap :: (Lift w a, PartLift w a ~ f p, Functor f) => a -> (p -> p) -> w -> w
liftMap a f = a >&> fmap f

set,(>@>) :: (Lift w a, PartLift w a ~ p) => a -> p -> w -> w
set a n = a >&> const n
(>@>) = set

-- Accessors for lists, tuples, etc
data Whole = Whole
instance Grab a Whole where
  type PartGrab a Whole = a
  grab _ = id
instance Lift a Whole where
  type PartLift a Whole = a
  lift _ = id

-- This is why we have UndecidableInstances enabled: p is not needed in the instance head because it is skipped over, however it does need to be in the context so we can make sure it is the same
{-
data Compose a b = Compose a b
instance (Grab w b, PartGrab w b ~ p, Grab p a, PartGrab p a ~ s) => Grab w (Compose a b) where
  type PartGrab w (Compose a b) = s
  grab (Compose a b) = grab a . grab b
instance (Lift w b, PartLift w b ~ p, Lift p a, PartLift p a ~ s) => Lift w (Compose a b) where
  type PartLift w (Compose a b) = s
  lift (Compose a b) f = b >&> (a >&> f)
-}

-- This is for explicity writing out your access functions, so you can do them inline, apply partially, etc
instance w ~ w' => Grab w (w' -> p) where
  type PartGrab w (w' -> p) = p
  grab = id
instance (w ~ w', w ~ w'', p ~ p') => Lift w ((p -> p') -> w' -> w'') where
  type PartLift w ((p -> p') -> w' -> w'') = p
  lift = id

data Standard = Standard
instance Functor f => Lift (f a) Standard where
  type PartLift (f a) Standard = a
  lift _ = fmap

-- Lets you swizzle different parts of the same type in the same whole type
swizzle :: (Grab w a, PartGrab w a ~ p, Lift w b, PartLift w b ~ p) => a -> b -> w -> w
swizzle fromA toA input = toA >@> (fromA ~>> input) $ input
-- swizzle = flip flip id . (ap .) . flip ((.) . set) . grab
-- why not

data Zero = Zero
data Succ z = Succ z

type One = Succ Zero
one :: One
one = Succ Zero
type Two = Succ One
two :: Two
two = Succ one
type Three = Succ Two
three :: Three
three = Succ two

-- Pairs:
instance Grab (a,b) One where
  type PartGrab (a,b) One = a
  grab _ = fst
instance Lift (a,b) One where
  type PartLift (a,b) One = a
  lift _ f (x,y) = (f x,y)

instance Grab (a,b) Two where
  type PartGrab (a,b) Two = b
  grab _ = snd
instance Lift (a,b) Two where
  type PartLift (a,b) Two = b
  lift _ f (x,y) = (x,f y)

-- Triples:
instance Grab (a,b,c) One where
  type PartGrab (a,b,c) One = a
  grab _ (x,_,_) = x
instance Lift (a,b,c) One where
  type PartLift (a,b,c) One = a
  lift _ f (x,y,z) = (f x,y,z)

instance Grab (a,b,c) Two where
  type PartGrab (a,b,c) Two = b
  grab _ (_,y,_) = y
instance Lift (a,b,c) Two where
  type PartLift (a,b,c) Two = b
  lift _ f (x,y,z) = (x,f y,z)

instance Grab (a,b,c) Three where
  type PartGrab (a,b,c) Three = c
  grab _ (_,_,z) = z
instance Lift (a,b,c) Three where
  type PartLift (a,b,c) Three = c
  lift _ f (x,y,z) = (x,y,f z)

-- Four-Tuples:
instance Grab (a,b,c,d) One where
  type PartGrab (a,b,c,d) One = a
  grab _ (x,_,_,_) = x
instance Lift (a,b,c,d) One where
  type PartLift (a,b,c,d) One = a
  lift _ f (x,y,z,w) = (f x,y,z,w)

instance Grab (a,b,c,d) Two where
  type PartGrab (a,b,c,d) Two = b
  grab _ (_,y,_,_) = y
instance Lift (a,b,c,d) Two where
  type PartLift (a,b,c,d) Two = b
  lift _ f (x,y,z,w) = (x,f y,z,w)

instance Grab (a,b,c,d) Three where
  type PartGrab (a,b,c,d) Three = c
  grab _ (_,_,z,_) = z
instance Lift (a,b,c,d) Three where
  type PartLift (a,b,c,d) Three = c
  lift _ f (x,y,z,w) = (x,y,f z,w)

instance Grab (a,b,c,d) (Succ Three) where
  type PartGrab (a,b,c,d) (Succ Three) = d
  grab _ (_,_,_,w) = w
instance Lift (a,b,c,d) (Succ Three) where
  type PartLift (a,b,c,d) (Succ Three) = d
  lift _ f (x,y,z,w) = (x,y,z,f w)

-- Fix instances: 
-- :%s/instance Access\(.*\)\n  grab\(.*\)\n  lift\(.*\)/instance Grab\1\r  grab\2\r  instance Lift\1\r  lift\3

{-
Example of how to set up your own datatype to work with accessors:

data Kitten = Kit {
  _age :: Integer,
  _name :: String
  } deriving Show

-- You can probably Template Haskell away the rest, but here it is anyway:
-- Beacuse I'm bad at TH :(

data Age = Age
instance Grab Kitten Age where
  type PartGrab Kitten Age = Integer
  grab _ = _age
instance Lift Kitten Age where
  type PartLift Kitten Age = Integer
  lift _ f k = k {_age = f $ _age k}

data Name = Name
instance Grab Kitten Name where
  type PartGrab Kitten Name = String
  grab _ = _name
instance Lift Kitten Name where
  type PartLift Kitten Name = String
  lift _ f k = k {_name = f $ _name k}

-}
