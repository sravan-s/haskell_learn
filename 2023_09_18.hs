---
-- functor
--
--
{-
Functor
---
type Functor :: (* -> *) -> Constraint
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a

3 <$ [1,2,3,4,5] = [3, 3, 3, 3, 3]
<$ is replace by operator

So functor satisfies -> fmap and replace-by (<$)
Some guy tell me all functors in haskell are endo-functors
ie -> represented by the Functor type class in Haskell.

map is a functor on lists
-}

{-
 - Applicative
 -    class (Functor f) => Applicative f where  
        pure :: a -> f a  
        (<*>) :: f (a -> b) -> f a -> f b

  Applicative is a functor that has pure, ie -> applicative 1 = [1] / applicative 3 = Just(3)
  Box the simple varable into execution context

  and product operator (<*>)
  [(*2), (+1)] <*> [10, 20, 30] = [20, 40, 60, 11, 21, 31]
  [(+1)] <*> [10, 20, 30] = [11, 21, 31]

 - -}

-- (*) <$> [2,5,10] <*> [8,10,11] 
-- [16,20,22,40,50,55,80,100,110]

-- filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]
-- <$> is infix of fmap
-- filter (>50) $ (fmap (*) [2, 5, 10]) <*> [8, 10, 11] 

-- import Control.Applicative
-- getZipList $ ZipList [(+1),(*100),(*5)] <*> ZipList [1,2,3]
-- [2, 200, 15]

myAction :: IO String  
myAction = (++) <$> getLine <*> getLine

myAction2 :: IO String
myAction2 = fmap (++) getLine <*> getLine

-- this one maps both variables
newtype Pair a = Pair { getPair :: (a,a) } deriving(Show)
instance Functor Pair where
  fmap f (Pair (x,y)) = Pair (f x, f y)

-- this one maps only l
newtype PairL r l = PairL { getPairL :: (l, r) }
instance Functor (PairL r) where
  fmap f (PairL (l,r)) = PairL (f l, r)

-- this one only maps r
newtype PairR l r = PairR { getPairR :: (l, r) }
instance Functor (PairR r) where
  fmap f (PairR (l,r)) = PairR (l, f r)

-- Note - In PairL - r is the locked variable, and in PairR l is the locked variable
--
{- Monoid -}

{-  
    class Monoid m where  
        mempty :: m  
        mappend :: m -> m -> m  
        mconcat :: [m] -> m  
        mconcat = foldr mappend mempty

  Using monoids to fold data structures
-}
