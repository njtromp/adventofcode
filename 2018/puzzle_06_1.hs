import Data.List

type ID = Int
type Coordinate = (Int,Int)

coordinates :: [Coordinate]
coordinates = [(124,262),(182,343),(79,341),(44,244),(212,64),(42,240),(225,195),(192,325),(192,318),(42,235),(276,196),(181,262),(199,151),(166,214),(49,81),(202,239),(130,167),(166,87),(197,53),(341,346),(235,241),(99,278),(163,184),(85,152),(349,334),(175,308),(147,51),(251,93),(163,123),(151,219),(162,107),(71,58),(249,293),(223,119),(46,176),(214,140),(80,156),(265,153),(92,359),(103,186),(242,104),(272,202),(292,93),(304,55),(115,357),(43,182),(184,282),(352,228),(267,147),(248,271)]
-- coordinates = [(1,1),(1,6),(8,3),(3,4),(5,5),(8,9)]

xCoordinates :: [Int]
xCoordinates = nub $ sort $ map fst coordinates 

yCoordinates :: [Int]
yCoordinates = nub $ sort $ map snd coordinates 

minX :: Int
minX = minimum xCoordinates

maxX :: Int
maxX = maximum xCoordinates

minY :: Int
minY = minimum yCoordinates

maxY :: Int
maxY = maximum yCoordinates

manhattenDistance :: Coordinate -> Coordinate -> Int
manhattenDistance (x,y) (x',y') = abs (x-x') + abs (y-y')

exclude :: Coordinate -> [Coordinate] -> [Coordinate]
exclude c = filter (/= c) 

isClosestTo :: Coordinate -> Coordinate -> Bool
isClosestTo c c' = all (\c'' -> manhattenDistance c c' < manhattenDistance c' c'') $ exclude c coordinates

countClosests :: Coordinate -> Int
countClosests c = length [ 1 | x <- [minX..maxX], y <- [minY..maxY], isClosestTo c (x,y)]

yBorder :: [Coordinate]
yBorder = nub $ sort [ c | x <- xCoordinates, c <- coordinates, isClosestTo c (x,minY) || isClosestTo c (x,maxY) ]

xBorder :: [Coordinate]
xBorder = nub $ sort [ c | y <- yCoordinates, c <- coordinates, isClosestTo c (minX,y) || isClosestTo c (maxX,y) ]

border = nub $ sort (yBorder ++ xBorder)

candidates :: [Coordinate]
candidates = filter (\c -> not $ elem c border) coordinates

puzzle_06_1 :: Int
puzzle_06_1 = maximum $ map (\c -> countClosests c) candidates
