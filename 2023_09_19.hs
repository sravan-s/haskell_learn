-- Monad


 {- 
  -class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  (>>)   :: m a ->  m b         -> m b
  return ::   a                 -> m a
  - -}

{-  Representing failure using Maybe monad
    Nondeterminism using List monad to represent carrying multiple values
    State using State monad
    Read-only environment using Reader monad
    I/O using IO monad 
-}

{-     "It is said that there’s a curse with Monads.
 -     I’m not making this up and it’s called the “monad tutorial fallacy”
 -     and the legend says that when you finally understand them,
 -     you lose the ability to explain it to others." -}

-- To details
-- return :: a -> m a -> is similar to Pure, wraps a value
--
-- >>=, or bind .......
-- a. Takes a monad (m a)
-- b. A fn that takes unboxed value, return a boxed value (a -> mb)
-- c. return a boxed value(m b)

--- Or -- bind is a function that combines a monad instance m a with a computation
-- that produces another monad instance m b from a's to produce a new
-- monad instance m b
--

-- m >> n = m >>= \_ -> n
-- >> function is default;
-- `Just 4 >> Nothing` is Nothing;
-- Which is Just 4 >>= (\_ => Nothing)
-- `Just 4 >> Just 2` is Just 2
-- Which is Just 4 >>= (\_ => (Just 2))


-- do notation
-- Just 3 >>= (\x -> Just (show x ++ "!"))
-- Just "3!"
foo :: Maybe String  
foo = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))

foo2 :: Maybe String
foo2 = do
  x <- Just 3
  y <- Just "!"
  Just (show x ++ y)
-- Some code
--
--
type Birds = Int
type Pole = (Birds,Birds)

{- step 1
landLeft :: Birds -> Pole -> Pole
landLeft n (l, r) = (l + n, r)

landRight :: Birds -> Pole -> Pole
landRight n (l, r) = (l, r + n)
-}
landLeft :: Birds -> Pole -> Maybe Pole  
landLeft n (left,right)  
    | abs ((left + n) - right) < 4 = Just (left + n, right)  
    | otherwise                    = Nothing  
  
landRight :: Birds -> Pole -> Maybe Pole  
landRight n (left,right)  
    | abs (left - (right + n)) < 4 = Just (left, right + n)  
    | otherwise                    = Nothing

-- step 1
-- util fn from the book
-- x -: f = f x
-- (0,0) -: landLeft 1 -: landRight 1 -: landLeft 2

-- step 2
-- return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)
-- output = Nothing 

-- step 3 -steps on a banana
banana :: Pole -> Maybe Pole  
banana _ = Nothing
-- return (0,0) >>= landLeft 1 >>= banana >>= landRight 1
-- Nothing
--
routine :: Maybe Pole
routine = do  
  -- start <- return (0,0)
  let start = (0, 0)
  first <- landLeft 2 start
  -- Nothing
  second <- landRight 2 first
  landLeft 1 second

-- When we say something is non-deterministic,
-- we often mean that from a single state or situation,
-- there are multiple possible outcomes or next states. 
-- However, once you decide to consider all possible outcomes simultaneously,
-- you're effectively working with a deterministic system on the level of sets of outcomes.
-- For instance, in the list monad example in Haskell:
-- Given a computation like [1,2] >>= \n -> ['a','b'],
-- the non-deterministic result is [(1,'a'),(1,'b'),(2,'a'),(2,'b')].
--
listOfTuples :: [(Int,Char,(Int, Char))]  
listOfTuples = do
  n <- [1,2]
  ch <- ['a','b']
  p <- return (n, ch)
  return (n,ch,p)

-- MonadPlus type class is for monads that can also act as monoids
{- 
 -     class Monad m => MonadPlus m where  
        mzero :: m a  
        mplus :: m a -> m a -> m a

        mzero is mempty in monoid
        mplus is callback for the reducer / fold ie: mappend
-}

