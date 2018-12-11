import Data.List
import Text.Printf

size :: Int
size = 300-3
-- size = 5-3

powerLevel :: Int -> Int -> Int -> Int
powerLevel x y serial = (((rackId * y + serial) * rackId `div` 100) `mod` 10) - 5
                where rackId = x + 10
-- powerLevel x y _ = xss !! (y-1) !! (x-1)
--                    where xss = [[-3,4,2,2,2],[-4,4,3,3,4],[-5,3,3,4,-4],[ 4,3,3,4,-3],[ 3,3,3,-5,-1]]
--                    where xss = [[-2,-4,4,4,4],[-4,4,4,4,-5],[ 4,3,3,4,-4],[ 1,1,2,4,-3],[-1,0,2,-5,-2]]

serial :: Int
serial = 7347
-- serial = 18

type Pos = (Int,Int)

rangeLevel :: [(Int,Pos)]
rangeLevel = [ (sum [powerLevel (x+x') (y+y') serial | x' <- [0..2], y' <- [0..2]], (x,y)) | x <- [1..size], y <- [1..size]]

puzzle_11_1 :: Pos
puzzle_11_1 = snd $ maximum rangeLevel

size' :: Int
size' = 300
-- size' = 5

powerLevel' :: Int -> Int -> Int -> Int
powerLevel' x y s = powerLevel (x+s) (y+s) serial + sum [powerLevel (x+x') (y+s) serial | x' <- [0..(s-1)]] + sum [powerLevel (x+s) (y+y') serial | y' <- [0..(s-1)]]

rangeLevel' :: Pos -> Int -> Int -> [(Int,Pos,Int)]
rangeLevel' (x,y) s p | x + (s-1) >= size' = []
                      | y + (s-1) >= size' = []
                      | otherwise     = [ (p+powerLevel' x y s,(x,y),s+1)] ++ rangeLevel' (x,y) (s+1) (p+powerLevel' x y s)

puzzle_11_2 :: (Int,Pos,Int)
puzzle_11_2 = maximum [ maximum $ rangeLevel' (x,y) 0 0 | x <- [1..size'], y <- [1..size']]

show' :: (Int,Pos,Int) -> String
show' (_,p,s) = (show $ fst p) ++ "," ++ (show $ snd p) ++ "," ++ (show s)

main = putStrLn (show' puzzle_11_2)

