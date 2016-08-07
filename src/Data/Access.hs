{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Access where

class Access w p a | w a -> p where
  grab :: a -> w -> p
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
(~>>) :: Access w p a => a -> w -> p
(~>>) = grab

(<<~) :: Access w p a => w -> a -> p
(<<~) = flip grab

(>&>) :: Access w p a => a -> (p -> p) -> w -> w
(>&>) = lift
infixr 6 >&>

liftMap :: (Access w (f p) a,Functor f) => a -> (p -> p) -> w -> w
liftMap a f = a >&> fmap f

set,(>@>) :: Access w p a => a -> p -> w -> w
set a n = a >&> const n
(>@>) = set

-- Accessors for lists, tuples, etc
data Whole = Whole
instance Access a a Whole where
  grab _ = id
  lift _ = id

-- This is why we have UndecidableInstances enabled: p is not needed in the instance head because it is skipped over, however it does need to be in the context so we can make sure it is the same
data Compose a b = Compose a b
instance (Access w p b, Access p s a) => Access w s (Compose a b) where
  grab (Compose a b) = grab a . grab b
  lift (Compose a b) f = b >&> (a >&> f)

-- This is for explicity writing out your access functions, so you can do them inline, apply partially, etc
data Explicate w p = Explicate (w -> p) ((p -> p) -> w -> w)
instance Access w p (Explicate w p) where
  grab (Explicate g _) = g
  lift (Explicate _ l) = l

-- Lets you swizzle different parts of the same type in the same whole type
swizzle :: (Access w p a, Access w p b) => a -> b -> w -> w
swizzle fromA toA input = toA >@> (fromA ~>> input) $ input
-- swizzle = flip flip id . (ap .) . flip ((.) . set) . grab
-- why not

data Zero = Zero
data Succ z = Succ z

type One = Succ Zero
type Two = Succ One
type Three = Succ Two

-- Pairs:
instance Access (a,b) a One where
  grab _ = fst
  lift _ f (x,y) = (f x,y)
instance Access (a,b) b Two where
  grab _ = snd
  lift _ f (x,y) = (x,f y)

-- Triples:
instance Access (a,b,c) a One where
  grab _ (x,_,_) = x
  lift _ f (x,y,z) = (f x,y,z)
instance Access (a,b,c) b Two where
  grab _ (_,y,_) = y
  lift _ f (x,y,z) = (x,f y,z)
instance Access (a,b,c) c Three where
  grab _ (_,_,z) = z
  lift _ f (x,y,z) = (x,y,f z)

-- Four-Tuples:
instance Access (a,b,c,d) a One where
  grab _ (x,_,_,_) = x
  lift _ f (x,y,z,w) = (f x,y,z,w)
instance Access (a,b,c,d) b Two where
  grab _ (_,y,_,_) = y
  lift _ f (x,y,z,w) = (x,f y,z,w)
instance Access (a,b,c,d) c Three where
  grab _ (_,_,z,_) = z
  lift _ f (x,y,z,w) = (x,y,f z,w)
instance Access (a,b,c,d) d (Succ Three) where
  grab _ (_,_,_,w) = w
  lift _ f (x,y,z,w) = (x,y,z,f w)


{-
Example of how to set up your own datatype to work with accessors:

data Kitten = Kit {
  _age :: Integer,
  _name :: String
  } deriving Show

-- You can probably Template Haskell away the rest, but here it is anyway:
-- Beacuse I'm bad at TH :(

data Age = Age
instance Access Kitten Integer Age where
  grab _ = _age
  lift _ f k = k {_age = f $ _age k}

data Name = Name
instance Access Kitten String Name where
  grab _ = _name
  lift _ f k = k {_name = f $ _name k}

-}
