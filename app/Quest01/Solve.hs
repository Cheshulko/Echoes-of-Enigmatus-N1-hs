module Quest01.Solve (solve) where

import           Control.Exception (assert)
import qualified Data.Map          as M
import           Text.Printf       (printf)
import           Text.Regex.TDFA   ((=~))

questPrefix :: String
questPrefix = "app/Quest01/input/%s"

data Params = Params {
    getA :: Int,
    getB :: Int,
    getC :: Int,
    getX :: Int,
    getY :: Int,
    getZ :: Int,
    getM :: Int
} deriving (Show)

type Input = [Params]

pattern :: String
pattern = "A=([0-9]*) B=([0-9]*) C=([0-9]*) X=([0-9]*) Y=([0-9]*) Z=([0-9]*) M=([0-9]*)"

parseParams :: String -> Params
parseParams line =
    case line =~ pattern :: (String, String, String, [String]) of
        (_, _, _, [a', b', c', x', y', z', m']) ->
            Params
                (read a')
                (read b')
                (read c')
                (read x')
                (read y')
                (read z')
                (read m')
        _ -> error "Invalid input"

parseInput :: FilePath -> IO Input
parseInput filePath = do
    content <- lines <$> readFile filePath

    return $ map parseParams content

solve :: String -> IO ()
solve part@"part1_sample"   = solvePart part1 part
solve part@"part1"          = solvePart part1 part
solve part@"part2_sample_1" = solvePart part2 part
solve part@"part2_sample_2" = solvePart part2 part
solve part@"part2"          = solvePart part2 part
solve part@"part3_sample_1" = solvePart part3 part
solve part@"part3_sample_2" = solvePart part3 part
solve part@"part3"          = solvePart part3 part
solve "test"                = test

solvePart solver part = printf "Part `%s`: %s\n" part . (show . solver) =<< parseInput (printf questPrefix part)

eni :: Int -> Int -> Int -> ([Int], [Int])
eni n exp m = break (== next) $ reverse xs'
    where
        getNext :: [Int] -> Int
        getNext xs = (head xs * n) `mod` m

        (xs, _) = until (\(xs, mem) -> M.member (getNext xs) mem) fn ([1], M.empty)
        xs'     = init xs
        next    = getNext xs'

        fn (xs, mem) = (next : xs, M.insert next ind mem)
            where
                ind  = length xs - 1
                next = getNext xs

build :: [Int] -> Int
build xs = read $ concatMap show xs

part1 :: Input -> Int
part1 input = maximum $ map solve input
    where
        solveOne :: Int -> Int -> Int -> [Int]
        solveOne n exp m = (reverse . take exp) $ beforeCycle ++ cycle cycle'
            where
                (beforeCycle, cycle') = eni n exp m

        solve :: Params -> Int
        solve params = build p1 + build p2 + build p3
            where
                p1 = solveOne (getA params) (getX params) (getM params)
                p2 = solveOne (getB params) (getY params) (getM params)
                p3 = solveOne (getC params) (getZ params) (getM params)

part2 :: Input -> Int
part2 input = maximum $ map solve input
    where
        toTake :: Int
        toTake = 5

        solveOne :: Int -> Int -> Int -> [Int]
        solveOne n exp m = take (min toTake exp) (afterCyclePart ++ cyclePart ++ beforeCyclePart)
            where
                (beforeCycle, cycle') = eni n exp m

                cycleLen        = length cycle'
                beforeCycleLen  = length beforeCycle

                leftForCycle    = max 0 (max exp toTake - beforeCycleLen)

                modCnt          = leftForCycle `mod` cycleLen
                divCnt          = leftForCycle `div` cycleLen

                beforeCyclePart = reverse beforeCycle
                cyclePart       = concat $ replicate divCnt (reverse cycle')
                afterCyclePart  = reverse $ take modCnt cycle'

        solve :: Params -> Int
        solve params = build p1 + build p2 + build p3
            where
                p1 = solveOne (getA params) (getX params) (getM params)
                p2 = solveOne (getB params) (getY params) (getM params)
                p3 = solveOne (getC params) (getZ params) (getM params)

part3 :: Input -> Int
part3 input = maximum $ map solve input
    where
        solveOne :: Int -> Int -> Int -> Int
        solveOne n exp m = sumBeforeCycle + sumCycle + sumAfterCycle
            where
                (beforeCycle, cycle') = eni n exp m

                cycleLen       = length cycle'
                beforeCycleLen = length beforeCycle

                leftForCycle   = max 0 (exp - beforeCycleLen)

                modCnt         = leftForCycle `mod` cycleLen
                divCnt         = leftForCycle `div` cycleLen

                sumBeforeCycle = sum $ take (min exp beforeCycleLen) beforeCycle
                sumCycle       = divCnt * sum cycle'
                sumAfterCycle  = sum $ reverse $ take modCnt cycle'

        solve :: Params -> Int
        solve params = p1 + p2 + p3
            where
                p1 = solveOne (getA params) (getX params) (getM params)
                p2 = solveOne (getB params) (getY params) (getM params)
                p3 = solveOne (getC params) (getZ params) (getM params)

testCase solver part result = do
   x <- solver <$> parseInput (printf questPrefix part)
   printf "%s result: %s\n" part (show x)
   assert (x == result) (printf "\tOK: %s\n" $ show x)

test :: IO ()
test = do
   -- Part 1
   testCase part1 "part1_sample"  11611972920
   testCase part1 "part1" 6532295480

   -- Part 2
   testCase part2 "part2_sample_1" 11051340
   testCase part2 "part2_sample_2" 1507702060886
   testCase part2 "part2" 144146590638054

   -- Part 3
   testCase part3 "part3_sample_1" 3279640
   testCase part3 "part3_sample_2" 7276515438396
   testCase part3 "part3" 570502266662551
