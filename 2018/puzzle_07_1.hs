import Data.List
import Data.Char

stepOrder :: [(Char,Char)]
-- stepOrder = [('V','H'),('U','R'),('E','D'),('B','R'),('W','X'),('A','P'),('T','L'),('F','C'),('P','Y'),('N','G'),('R','S'),('D','C'),('O','K'),('L','J'),('J','H'),('M','I'),('G','K'),('Z','Q'),('X','Q'),('H','I'),('K','Y'),('Q','S'),('I','Y'),('S','Y'),('C','Y'),('T','S'),('P','S'),('I','S'),('V','O'),('O','Q'),('T','R'),('E','J'),('F','S'),('O','H'),('Z','S'),('D','Z'),('F','K'),('W','P'),('G','I'),('B','T'),('G','Y'),('X','S'),('B','K'),('V','A'),('U','N'),('T','P'),('V','D'),('G','X'),('B','D'),('R','J'),('M','Z'),('U','Z'),('U','G'),('A','C'),('H','Q'),('X','K'),('B','S'),('Q','C'),('Q','Y'),('R','I'),('V','Q'),('A','D'),('D','S'),('K','S'),('G','C'),('D','O'),('R','H'),('K','Q'),('W','R'),('H','Y'),('P','J'),('N','Z'),('J','K'),('W','M'),('A','Z'),('V','W'),('J','X'),('U','F'),('P','L'),('W','G'),('T','F'),('R','C'),('R','O'),('Z','C'),('E','S'),('L','I'),('U','O'),('W','K'),('K','I'),('O','M'),('V','M'),('V','Z'),('A','I'),('F','J'),('F','O'),('M','C'),('Q','I'),('H','S'),('U','A'),('J','S'),('P','Z')]
stepOrder = [('C','A'),('C','F'),('A','B'),('A','D'),('B','E'),('D','E'),('F','E')]

clean :: [Char] -> [Char]
clean xs = nub $ sort xs

reachableSteps :: Char -> [Char]
reachableSteps c = clean [ f | (s,f) <- stepOrder, s == c ]

priorSteps :: Char -> [Char]
priorSteps c = clean [ s | (s,f) <- stepOrder, f == c ]

start :: [Char]
start = (clean [ s | (s,_) <- stepOrder ]) \\ (clean [ f | (_,f) <- stepOrder ])

takeStep :: [Char] -> [Char]-> Int
takeStep [] ys = ys
takeStep xs ys = takeStep xs' (ys ++ [x])
                 where xs' = clean ((delete x xs) ++ (reachableSteps x))
                       x = head [ s | s <- xs, priorSteps s ==  intersect (priorSteps s) ys ]

puzzle_07_1 :: String
puzzle_07_1 = takeStep start []