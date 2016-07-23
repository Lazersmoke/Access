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

swizzle :: (Access w p a, Access w p b) => a -> b -> w -> w
swizzle fromA toA input = toA >@> (fromA ~>> input) $ input
-- swizzle = flip flip id . (ap .) . flip ((.) . set) . grab
-- why not

-- TODO: Check law satisfaction on AccessHead and AccessTail
data AccessHead = AccessHead
instance Monoid a => Access [a] a AccessHead where
  grab _ (x:_) = x
  grab _ [] = mempty 
  lift _ f (x:xs) = f x : xs
  lift _ _ [] = []

data AccessTail = AccessTail
instance Access [a] [a] AccessTail where
  grab _ (_:xs) = xs
  grab _ [] = []
  lift _ f (x:xs) = x : f xs
  lift _ _ [] = []

data First = First
instance Access (a,b) a First where
  grab _ = fst
  lift _ f (x,y) = (f x,y)
instance Access (a,b,c) a First where
  grab _ (x,_,_) = x
  lift _ f (x,y,z) = (f x,y,z)

data Second = Second
instance Access (a,b) b Second where
  grab _ = snd
  lift _ f (x,y) = (x,f y)
instance Access (a,b,c) b Second where
  grab _ (_,y,_) = y
  lift _ f (x,y,z) = (x,f y,z)

data Third = Third
instance Access (a,b,c) c Third where
  grab _ (_,_,z) = z
  lift _ f (x,y,z) = (x,y,f z)


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
