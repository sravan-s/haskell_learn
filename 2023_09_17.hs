import System.IO
import Control.Monad
-- London to heathrow

data Section = Section {
  divider :: Int
  , toA :: Int
  , toB :: Int
} deriving (Show)

data Path = Path {
  distance :: Int
  , secNo :: Int
  , lane :: Char
} deriving (Show)

-- We dont do error handling
toSections :: [Int] -> [Section]
toSections [x] = []
toSections (x:y:z:xs) = Section { divider = x, toA = y, toB = z }: toSections xs


parseInput :: String -> [Section]
parseInput =  toSections . (0:) . fmap read . lines
  -- We assume 0 is distance between  A0 and B0 -> ie- > [30, 90, ...] -> [0, 30, 90, ...]
  --

-- [Path is a stack for ease]
predicate :: Path -> Section -> Path
predicate Path{ distance = 0 } section 
  | toA section > toB section = Path{ distance = toB section, lane = 'B', secNo = 0 }
  | otherwise = Path{ distance = toA section, lane = 'A', secNo = 0 }

predicate p section
  | prevLane == 'A' && toAVal <= toBVal + dividerVal = Path{ distance = prevDistance + toAVal, lane = 'A', secNo = prevSecNo + 1 }
  | prevLane == 'A' && toAVal > toBVal + dividerVal  = Path{ distance = prevDistance + toBVal + dividerVal, lane = 'B', secNo = prevSecNo + 1 }
  | prevLane == 'B' && toBVal <= toAVal + dividerVal = Path{ distance = prevDistance + toBVal, lane = 'B', secNo = prevSecNo + 1 }
  | prevLane == 'B' && toBVal > toAVal + dividerVal  = Path{ distance = prevDistance + toAVal + dividerVal, lane = 'A', secNo = prevSecNo + 1 }
  where 
    prevDistance = distance p
    prevSecNo = secNo p
    prevLane = lane p
    toAVal = toA section
    toBVal = toB section
    dividerVal = divider section

-- scanl (b -> a -> b) -> b -> [a] -> [b]
compute :: [Section] -> [Path]
compute = scanl predicate Path { distance = 0, lane = 'A', secNo = 0 }

main :: IO ()
main = do
  handle <- openFile "london_to_heathrow.txt" ReadMode
  contents <- hGetContents handle
  let output = compute (parseInput contents)
  -- we can print path from output list :)
  print output

