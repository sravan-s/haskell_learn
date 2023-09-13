-- zipWith Implementation
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' fn [] _ = []
zipWith' fn _ [] = []
zipWith' fn (x:xs) (y:ys) = (fn x y):(zipWith' fn xs ys)

{-  -}
flip' :: (a -> b -> c) -> b -> a -> c
flip' fn a b = fn b a

{- Map -}
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' fn (x:xs) = (fn x):(map fn xs)

-- Filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' fn (x:xs)
  | result == True = x:(filter' fn xs) -- yes yes, donot need to derive result seperately
  | otherwise = (filter' fn xs)
  where result = fn x

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
  let firstHalf = quickSort (filter (<= x) xs)
      secondHalf = quickSort (filter (>x) xs)
  in firstHalf ++ [x] ++ secondHalf

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' predicate (x:xs)
  | predicate x = x:(takeWhile' predicate xs)
  | otherwise = []

-- damn!!!
foldl' :: (accum -> li -> accum) -> accum -> [li] -> accum
foldl' _ accum [] = accum
foldl' fn accum (x:xs) = foldl' fn (fn accum x) xs

