module Quest03.Solve (solve) where

import           Control.Exception (assert)
import           Text.Printf       (printf)
import           Text.Regex.TDFA   ((=~))

questPrefix :: String
questPrefix = "app/Quest03/input/%s"

data Coord = Coord {
    getX :: Integer,
    getY :: Integer
} deriving Show

type Input = [Coord]

coordPattern :: String
coordPattern = "x=([0-9]*) y=([0-9]*)"

parseCoord :: String -> Coord
parseCoord line =
    case line =~ coordPattern :: (String, String, String, [String]) of
      (_, _, _, [x, y]) -> Coord (read x - 1) (read y - 1)
      _                 -> error "Wrong input format"

parseInput :: FilePath -> IO Input
parseInput filePath = do
    content <- lines <$> readFile filePath

    return $ map parseCoord content

solve :: String -> IO ()
solve part@"part1_sample"   = solvePart part1 part
solve part@"part1"          = solvePart part1 part
solve part@"part2_sample_1" = solvePart part2 part
solve part@"part2_sample_2" = solvePart part2 part
solve part@"part2"          = solvePart part2 part
solve part@"part3_sample"   = solvePart part3 part
solve part@"part3"          = solvePart part3 part
solve "test"                = test

solvePart solver part = printf "Part `%s`: %s\n" part . (show . solver) =<< parseInput (printf questPrefix part)

part1 :: Input -> Integer
part1 input = sum $ zipWith (\x y -> 1 + x + steps * (1 + y)) dxs dys
    where
        steps = 100
        discs = map (\coord -> getX coord + getY coord + 1) input
        dxs   = zipWith (\coord disc -> (getX coord + steps) `mod` disc) input discs
        dys   = zipWith (\coord disc -> ((disc - 1) * steps + getY coord) `mod` disc) input discs

part2 :: Input -> Integer
part2 input = fst $ foldl1 combine $ zip ys discs
    where
        discs = map (\coord -> getX coord + getY coord + 1) input
        ys    = map getY input

        extendedGcd :: Integer -> Integer -> (Integer, Integer, Integer)
        extendedGcd a 0 = (a, 1, 0)
        extendedGcd a b = (g, x, y)
            where
                (g, x1, y1) = extendedGcd b ((a + b) `mod` b)
                x           = y1
                y           = x1 - (a `div` b) * y1

        combine :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
        combine (a1, m1) (a2, m2)
            | (g + a2 - a1) `mod` g /= 0 = error "No solution"
            | otherwise                  = (x, lcm')
            where
                (g, s, t)   = extendedGcd m1 m2
                lcm'        = (m1 * m2) `div` g
                x           = (lcm' + a1 + (a2 - a1) `div` g * s * m1) `mod` lcm'

part3 :: Input -> Integer
part3 = part2

testCase solver part result = do
   x <- solver <$> parseInput (printf questPrefix part)
   printf "%s result: %s\n" part (show x)
   assert (x == result) (printf "\tOK: %s\n" $ show x)

test :: IO ()
test = do
   -- Part 1
   testCase part1 "part1_sample" 1310
   testCase part1 "part1" 4042

   -- Part 2
   testCase part2 "part2_sample_1" 14
   testCase part2 "part2_sample_2" 13659
   testCase part2 "part2" 1088901

   -- Part 3
   testCase part3 "part3" 90000078821
