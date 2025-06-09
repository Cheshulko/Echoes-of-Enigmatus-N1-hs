{-# LANGUAGE InstanceSigs #-}
module Quest02.Solve (solve) where

import           Control.Exception (assert)
import           Data.List         (find, transpose)
import           Data.Maybe        (fromJust)
import           Text.Printf       (printf)
import           Text.Regex.TDFA   ((=~))

questPrefix :: String
questPrefix = "app/Quest02/input/%s"

data Item = Item {
   getItemId :: Int,
   getRank   :: Int,
   getSymbol :: String
} deriving (Show, Eq)

data Node = Node {
   getItem      :: Item,
   getLeftNode  :: Maybe Node,
   getRightNode :: Maybe Node
} deriving (Show)

instance Eq Node where
   (==) :: Node -> Node -> Bool
   (Node item1 _ _) == (Node item2 _ _) =
      item1 == item2

data Forest = Forest {
   getLeft  :: Maybe Node,
   getRight :: Maybe Node
}

data Add = Add {
   getAddId     :: Int,
   getLeftItem  :: Item,
   getRightItem :: Item
} deriving (Show)

newtype Swap = Swap {
   getSwapId    :: Int
} deriving (Show)

data Command =
   AddCommand   Add |
   SwapCommand Swap

type Input = [Command]

addPattern :: String
addPattern = "ADD id=([0-9]*) left=\\[([0-9]*),([A-Z!]*)\\] right=\\[([0-9]*),([A-Z!]*)\\]"

parseAdd :: String -> Maybe Add
parseAdd line =
   case line =~ addPattern :: (String, String, String, [String]) of
      (_, _, _, [id', leftRank, leftSymbol, rightRank, rightSymbol]) ->
         Just $ Add
            (read id')
            (Item (read id') (read  leftRank)  leftSymbol)
            (Item (read id') (read rightRank) rightSymbol)
      _ -> Nothing

swapPattern :: String
swapPattern = "SWAP ([0-9]*)"

parseSwap :: String -> Maybe Swap
parseSwap line = case line =~ swapPattern :: (String, String, String, [String]) of
      (_, _, _, [id']) -> Just $ Swap $ read id'
      _                -> Nothing

parseCommand :: String -> Command
parseCommand line = case parseAdd  line of
   Just add     -> AddCommand add
   Nothing      ->  case parseSwap line of
      Just swap -> SwapCommand swap
      Nothing   -> error "Invalid input"

parseInput :: FilePath -> IO Input
parseInput filePath = do
   content <- lines <$> readFile filePath

   return $ map parseCommand content

solve :: String -> IO ()
solve part@"part1_sample_1" = solvePart part1 part
solve part@"part1_sample_2" = solvePart part1 part
solve part@"part1"          = solvePart part1 part
solve part@"part2_sample"   = solvePart part2 part
solve part@"part2"          = solvePart part2 part
solve part@"part3_sample_1" = solvePart part3 part
solve part@"part3_sample_2" = solvePart part3 part
solve part@"part3"          = solvePart part3 part
solve "test"                = test

solvePart solver part = printf "Part `%s`: %s\n" part . (show . solver) =<< parseInput (printf questPrefix part)

zipConcat :: [String] -> [String] -> [String]
zipConcat xs ys =
   take (max (length xs) (length ys)) $
   map concat $
   transpose [xs ++ repeat "", ys ++ repeat ""]

findNodes :: Int -> Maybe Node -> [Node]
findNodes _ Nothing = []
findNodes id' (Just node)
   | idCur == id' = [node]
   | otherwise    =
         findNodes id' (getLeftNode  node) ++
         findNodes id' (getRightNode node)
   where
      idCur = getItemId $ getItem node

updateNode :: Bool -> Node -> Node -> Maybe Node -> Maybe Node
updateNode _ srcNode dstNode Nothing = Nothing
updateNode withSubtree srcNode dstNode (Just node)
   | srcNode == node && withSubtree = Just dstNode
   | srcNode == node                = Just $ node {getItem = item}
   | otherwise                      =
      Just $ node {
         getLeftNode  = updateNode withSubtree srcNode dstNode (getLeftNode  node),
         getRightNode = updateNode withSubtree srcNode dstNode (getRightNode node)
      }
   where
      item = getItem dstNode

insertItem :: Item -> Maybe Node -> Node
insertItem item Nothing = Node item Nothing Nothing
insertItem item (Just node)
   | rank' < rank = node {getLeftNode  = Just $ insertItem item $ getLeftNode  node }
   | rank' > rank = node {getRightNode = Just $ insertItem item $ getRightNode node }
   | otherwise    = error "How to handle equal ranks?"
   where
      rank' = getRank item
      rank  = getRank $ getItem node

handleCommand :: Bool -> Command -> Forest -> Forest
handleCommand _ (AddCommand add) forest =
   forest {getLeft = left', getRight = right'}
   where
      left'  = ((Just .) . insertItem .   getLeftItem) add (getLeft  forest)
      right' = ((Just .) . insertItem .  getRightItem) add (getRight forest)

handleCommand withSubtree (SwapCommand swap) forest = forest'
   where
      (left, right) = (getLeft forest, getRight forest)

      swapId        = getSwapId swap
      nodeLeft      = findNodes swapId  left
      nodeRight     = findNodes swapId right

      forest' = case (nodeLeft, nodeRight) of
         ([node1], [node2])   -> forest {
            getLeft  = updateNode withSubtree node1 node2 left,
            getRight = updateNode withSubtree node2 node1 right
         }
         ([node1, node2], []) -> forest {
            getLeft = Just $ swapSameTree left node1 node2
         }
         ([], [node1, node2]) -> forest {
            getRight = Just $ swapSameTree right node1 node2
         }

      swapSameTree :: Maybe Node -> Node -> Node -> Node
      swapSameTree root node1 node2 =
         let
            dummy = Node (Item (-1) (-1) "") Nothing Nothing
         in
            fromJust $
               updateNode True dummy node1 $
               updateNode True node1 node2 $
               updateNode True node2 dummy root

buildLevels :: Maybe Node -> [String]
buildLevels Nothing = []
buildLevels (Just node) = symbol : children
   where
      symbol   = getSymbol $ getItem node
      left     = buildLevels (getLeftNode  node)
      right    = buildLevels (getRightNode node)
      children = zipConcat left right

buildAnswer :: [[String]] -> String
buildAnswer [left, right] = left' ++ right'
   where
      maxLeft  = maximum $ map length  left
      maxRight = maximum $ map length right
      left'    = fromJust $ find ((== maxLeft) .  length)  left
      right'   = fromJust $ find ((== maxRight) . length) right

solver :: Bool -> Input -> [[String]]
solver withSubtree input = [leftLevels, rightLevels]
   where
      rootForest = Forest {
         getLeft = Nothing,
         getRight = Nothing
      }

      forest'       = foldr (handleCommand withSubtree) rootForest $ reverse input
      left          = getLeft forest'
      right         = getRight forest'

      leftLevels    = buildLevels left
      rightLevels   = buildLevels right

part1 :: Input -> String
part1 = buildAnswer . solver False

part2 :: Input -> String
part2 = part1

part3 :: Input -> String
part3 = buildAnswer . solver True

testCase solver part result = do
   x <- solver <$> parseInput (printf questPrefix part)
   printf "%s result: %s\n" part (show x)
   assert (x == result) (printf "\tOK: %s\n" $ show x)

test :: IO ()
test = do
   -- Part 1
   testCase part1 "part1_sample_1" "CFGNLK"
   testCase part1 "part1_sample_2" "EVERYBODYCODES"
   testCase part1 "part1" "QUACK!MXFJXVHR"

   -- Part 2
   testCase part2 "part2_sample" "MGFLNK"
   testCase part2 "part2" "QUACK!NZRJTFJZLMFLSJ"

   -- Part 3
   testCase part3 "part3_sample_1" "DJMGL"
   testCase part3 "part3_sample_2" "DJCGL"
   testCase part3 "part3" "QUACK!GTZSLZGWHJZTGRLTYJYWVXPBWNZP"
