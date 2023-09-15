import Data.List
-- Reverse polish notation
--
-- "1 3 2 +  -" = 2 + 3 - 1 = 4
-- 
--

-- we only accept +, - now
compute:: (Num a, Read a) => [a] -> String -> [a]
compute (x:y:ys) "+" = (x + y):ys
compute (x:y:ys) "-" = (x - y):ys
compute xs numAsStr = read numAsStr:xs

solveRPN::(Num a, Read a) => String -> a
solveRPN = head . foldl compute [] . words

-- Todo: Shortest path?

