{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Access where

class Access w p a | w a -> p

instance (Grab w p a, Lift w p a) => Access w p a

class Grab w p a | w a -> p where
  grab :: a -> w -> p

class Lift w p a | w a -> p where
  lift :: a -> (p -> p) -> w -> w


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
(~>>) :: Grab w p a => a -> w -> p
(~>>) = grab

(<<~) :: Grab w p a => w -> a -> p
(<<~) = flip grab

(>&>) :: Lift w p a => a -> (p -> p) -> w -> w
(>&>) = lift
infixr 6 >&>

liftMap :: (Lift w (f p) a,Functor f) => a -> (p -> p) -> w -> w
liftMap a f = a >&> fmap f

set,(>@>) :: Lift w p a => a -> p -> w -> w
set a n = a >&> const n
(>@>) = set

-- Accessors for lists, tuples, etc
data Whole = Whole
instance Grab a a Whole where
  grab _ = id
instance Lift a a Whole where
  lift _ = id

-- This is why we have UndecidableInstances enabled: p is not needed in the instance head because it is skipped over, however it does need to be in the context so we can make sure it is the same
data Compose a b = Compose a b
instance (Grab w p b, Grab p s a) => Grab w s (Compose a b) where
  grab (Compose a b) = grab a . grab b
instance (Lift w p b, Lift p s a) => Lift w s (Compose a b) where
  lift (Compose a b) f = b >&> (a >&> f)

-- This is for explicity writing out your access functions, so you can do them inline, apply partially, etc
instance (w ~ w', p ~ p') => Grab w p (w' -> p') where
  grab = id
instance (w ~ w', p ~ p', w ~ w'', p ~ p'') => Lift w p ((p' -> p'') -> w' -> w'') where
  lift = id

data Standard = Standard
instance Functor f => Lift (f a) a Standard where
  lift _ = fmap

-- Lets you swizzle different parts of the same type in the same whole type
swizzle :: (Grab w p a, Lift w p b) => a -> b -> w -> w
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
instance Grab (a,b) a One where
  grab _ = fst
instance Lift (a,b) a One where
  lift _ f (x,y) = (f x,y)

instance Grab (a,b) b Two where
  grab _ = snd
instance Lift (a,b) b Two where
  lift _ f (x,y) = (x,f y)

-- Triples:
instance Grab (a,b,c) a One where
  grab _ (x,_,_) = x
instance Lift (a,b,c) a One where
  lift _ f (x,y,z) = (f x,y,z)

instance Grab (a,b,c) b Two where
  grab _ (_,y,_) = y
instance Lift (a,b,c) b Two where
  lift _ f (x,y,z) = (x,f y,z)

instance Grab (a,b,c) c Three where
  grab _ (_,_,z) = z
instance Lift (a,b,c) c Three where
  lift _ f (x,y,z) = (x,y,f z)

-- Four-Tuples:
instance Grab (a,b,c,d) a One where
  grab _ (x,_,_,_) = x
instance Lift (a,b,c,d) a One where
  lift _ f (x,y,z,w) = (f x,y,z,w)

instance Grab (a,b,c,d) b Two where
  grab _ (_,y,_,_) = y
instance Lift (a,b,c,d) b Two where
  lift _ f (x,y,z,w) = (x,f y,z,w)

instance Grab (a,b,c,d) c Three where
  grab _ (_,_,z,_) = z
instance Lift (a,b,c,d) c Three where
  lift _ f (x,y,z,w) = (x,y,f z,w)

instance Grab (a,b,c,d) d (Succ Three) where
  grab _ (_,_,_,w) = w
instance Lift (a,b,c,d) d (Succ Three) where
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
instance Grab Kitten Integer Age where
  grab _ = _age
instance Lift Kitten Integer Age where
  lift _ f k = k {_age = f $ _age k}

data Name = Name
instance Grab Kitten String Name where
  grab _ = _name
instance Lift Kitten String Name where
  lift _ f k = k {_name = f $ _name k}

-}
