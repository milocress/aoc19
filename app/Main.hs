module Main where

main :: IO ()
main = do
  nums <- fmap read . lines <$> readFile "res/day1.txt"
  print . sum $ fuel <$> nums
  print . sum $ allFuel' <$> nums

fuel :: Int -> Int
fuel = (+ (-2)) . (floor :: Double -> Int) . (/ 3) . fromIntegral

allFuel :: Int -> Int
allFuel mass = case compare (fuel mass) 0 of
  LT -> mass
  EQ -> mass
  GT -> mass + allFuel (fuel mass)

allFuel' = allFuel . fuel
