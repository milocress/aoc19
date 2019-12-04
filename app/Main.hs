module Main where

import Control.Monad.State (State, modify, gets, evalState)
import Control.Monad (guard)
import Data.List (elemIndex, sortOn, group)
import Data.List.Split (splitOn)
import Data.Set (Set, fromList, findMin, intersection, member)
import qualified Data.Set as S (map)
import Data.Maybe (fromJust)

main :: IO ()
main = day4

day1 :: IO ()
day1 = do
  nums <- fmap read . lines <$> readFile "res/day1.txt"
  print . sum $ fuel <$> nums
  print . sum $ allFuel' <$> nums

fuel :: Int -> Int
fuel = (+ (-2)) . (floor :: Double -> Int) . (/ 3) . fromIntegral

allFuel, allFuel' :: Int -> Int
allFuel mass = case compare (fuel mass) 0 of
  LT -> mass
  EQ -> mass
  GT -> mass + allFuel (fuel mass)

allFuel' = allFuel . fuel

type Index = Int
type ProgramData = Int
type Program = [ProgramData]
type ProgramState = State (Int, Program)

getProgram :: ProgramState Program
getProgram = gets snd

nextInt :: ProgramState ProgramData
nextInt = do
  i <- idx
  p <- getProgram
  tick
  return $ p !! i

tick :: ProgramState ()
tick = modify $ \(a, p) -> (succ a, p)

idx :: ProgramState Index
idx = gets fst

getIndex :: Index -> ProgramState ProgramData
getIndex i = (!! i) <$> getProgram

replace :: Program -> Index -> ProgramData -> Program
replace p i d = a <> [d] <> tail as where
  (a,as) = splitAt i p

updateState :: Index -> ProgramData -> ProgramState ()
updateState i d = modify $ \(a, p) -> (a, replace p i d)

data Outcome = Continue | Stop

op :: (ProgramData -> ProgramData -> ProgramData) -> ProgramState ()
op f = do
  i <- nextInt
  j <- nextInt
  ptr <- nextInt
  x <- getIndex i
  y <- getIndex j
  updateState ptr (f x y)

step :: ProgramState Outcome
step = do
  opCode <- nextInt
  case opCode of
    1  -> do
      op (+)
      return Continue
    2  -> do
      op (*)
      return Continue
    99 -> return Stop
    _  -> error "ya done goofed"

runProgram :: ProgramState ()
runProgram = do
  outcome <- step
  case outcome of
    Continue -> runProgram
    Stop     -> return ()

parseProgram :: String -> Program
parseProgram = map read . splitOn ","

prepareProgram :: ProgramState ()
prepareProgram = do
  updateState 1 12
  updateState 2 2

getPositionZero :: ProgramState ProgramData
getPositionZero = head <$> getProgram

day2 :: IO ()
day2 = do
  program <- parseProgram <$> readFile "res/day2.txt"
  print $ evalState (prepareProgram >> runProgram >> getPositionZero) (0, program)
  let [(noun, verb)] = findCorrectPrep program
  print $ 100 * noun + verb

preparations :: [ProgramState (ProgramData, ProgramData)]
preparations = do
  x <- [0..99]
  y <- [0..99]
  return $ do
    updateState 1 x
    updateState 2 y
    return (x, y)

findCorrectPrep :: Program -> [(ProgramData, ProgramData)]
findCorrectPrep program = do
  prep <- preparations
  guard $ evalState (prep >> runProgram >> getPositionZero) (0, program) == 19690720
  return $ evalState prep (0, program)

day3 :: IO ()
day3 = do
  pathStrings <- lines <$> readFile "res/day3.txt"
  let paths = parsePath <$> pathStrings
      positions = foldl1 intersection $ trace <$> paths
      distances = S.map manhattan positions
      allPositions = zip [1..] . traceSteps <$> paths
      wantedPositions = map fst . sortOn snd . filter (\pos -> snd pos `member` positions) <$> allPositions
      bestSteps = minimum $ zipWith (+) (head wantedPositions) (wantedPositions !! 1)
  print $ findMin distances
  print (bestSteps :: Int)

rankPosition :: Position -> [Position] -> Int
rankPosition p = succ . fromJust . elemIndex p

data Direction = U | R deriving Show
data Instruction = I Direction Int deriving Show
type Position = (Int, Int)

manhattan :: Position -> Int
manhattan (a, b) = abs a + abs b

move :: Position -> Instruction -> ([Position], Position)
move (x, y) (I d n) = case d of
  U -> ((\dy -> (x, y + dy)) <$> ns, (x, y + n))
  R -> ((\dx -> (x + dx, y)) <$> ns, (x + n, y))
  where ns = tail $ (signum n *) <$> [0 .. abs n]

type Path = [Instruction]

addFst :: Semigroup a => a -> (a, b) -> (a, b)
addFst ps (a, b) = (ps <> a, b)

trace :: Path -> Set Position
trace = fromList . traceSteps

traceSteps :: Path -> [Position]
traceSteps = fst . foldl (\(ps, p) -> addFst ps . move p) (mempty, (0,0))

parseInstruction :: String -> Instruction
parseInstruction (i:ns) = case i of
  'U' -> I U .          read $ ns
  'D' -> I U . negate . read $ ns
  'L' -> I R . negate . read $ ns
  'R' -> I R .          read $ ns
  _ -> error "Ya done goofed"
parseInstruction [] = error "Big bad things"

parsePath :: String -> Path
parsePath = map parseInstruction . splitOn ","


day4 :: IO ()
day4 = do
  print . length . passwords False $ parseRanges "240920" "789857"
  print . length . passwords True $ parseRanges "240920" "789857"

type Range = (Int, Int)
getRange :: Int -> [Int]
getRange l = [l..9]

defaultRange :: Range
defaultRange = (0, 9)

type Password = Int

passwords :: Bool -> Range -> [Password]
passwords strict (hi, lo) = do
  a <- getRange 1
  b <- getRange a
  c <- getRange b
  d <- getRange c
  e <- getRange d
  f <- getRange e
  let password = a * 100000 + b * 10000 + c * 1000 + d * 100 + e * 10 + f
  guard . between hi lo $ password
  guard . twoAdjacent $ password
  return password
  where
    between a b x = x >= a && x <= b
    twoAdjacent = if strict
      then any (\a -> length a == 2) . group . show
      else (6 >) . length . group . show

parseRanges :: String -> String -> Range
parseRanges a b = (read a, read b)
