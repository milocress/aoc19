module Main where

import Control.Monad.State (State, modify, gets, evalState)
import Control.Monad (guard)
import Data.List (splitAt)
import Data.List.Split (splitOn)

main :: IO ()
main = day2

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
