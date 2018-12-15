module Puzzle_14 where

import Data.Sequence
import Data.Maybe

recipeScores :: Seq Int
recipeScores = fromList [3,7]

newRecipeScores :: Int -> Int -> Seq Int
newRecipeScores x y = if r > 9 then singleton (r `div` 10) |> (r `mod` 10) else singleton r
                      where r = x + y

currentRecipes :: (Int,Int)
currentRecipes = (0,1)

nextCurrentRecipes :: (Int,Int) -> (Int,Int) -> Int -> (Int,Int)
nextCurrentRecipes (f,s) (f',s') l = ((f + 1 + f') `mod` l,(s + 1 + s') `mod` l)

lookup :: Int -> Seq Int ->  Int
lookup n xs = fromMaybe 0 $ Data.Sequence.lookup n xs

evolve :: ((Int,Int),Seq Int) -> ((Int,Int),Seq Int)
evolve ((f,s),rs) = (nextCurrentRecipes (f,s) (sf, ss) l, rs'')
                    where sf = Puzzle_14.lookup f rs
                          ss = Puzzle_14.lookup s rs
                          rs' = newRecipeScores sf ss
                          rs'' = rs >< rs'
                          l = Data.Sequence.length rs''

generateRecipes :: Int -> ((Int,Int),Seq Int) -> Seq Int
generateRecipes n ((f,s),rs) | n <= Data.Sequence.length rs = rs
                             | otherwise                    = generateRecipes n $ evolve ((f,s),rs)

puzzle_14_1 :: Int -> Seq Int
puzzle_14_1 n = Data.Sequence.take 10 $ Data.Sequence.drop n $ generateRecipes (n+10) (currentRecipes, recipeScores)
