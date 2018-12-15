import Data.List
import Data.Maybe

countPlants :: String -> Int
countPlants xs = length $ filter (=='#') xs

initialState :: String
initialState = "#.#####.##.###...#...#.####..#..#.#....##.###.##...#####.#..##.#..##..#..#.#.#.#....#.####....#..#"
-- initialState = "#..#.#..##......###...###"


transformations :: [(String, Char)]
transformations = [("#.#..",'.'),("..###",'.'),("...##",'.'),(".####",'#'),(".###.",'#'),("#....",'.'),("#.#.#",'.'),("###..",'#'),("#..#.",'.'),("#####",'#'),(".##.#",'#'),(".#...",'.'),("##.##",'#'),("#...#",'#'),(".#.##",'.'),("##..#",'.'),(".....",'.'),(".#.#.",'#'),("#.###",'#'),("....#",'.'),("...#.",'#'),("..#.#",'#'),("##...",'#'),("####.",'#'),("#..##",'#'),("##.#.",'#'),("###.#",'.'),("#.##.",'.'),("..#..",'#'),(".#..#",'.'),("..##.",'.'),(".##..",'#')]
-- transformations = [("...##",'#'),("..#..",'#'),(".#...",'#'),(".#.#.",'#'),(".#.##",'#'),(".##..",'#'),(".####",'#'),("#.#.#",'#'),("#.###",'#'),("##.#.",'#'),("##.##",'#'),("###..",'#'),("###.#",'#'),("####.",'#')]

generate :: String -> Char
generate xs = fromMaybe '.' $ lookup xs transformations

nextGeneration :: String -> String
nextGeneration xs | length xs < 5 = if elem '#' xs then drop 2 xs ++ ".." else drop 2 xs
                  | otherwise     = generate (take 5 xs) : nextGeneration (tail xs)

evolution :: String -> String
evolution xs = take 2 xs ++ nextGeneration xs

generateEvolution :: Int -> String -> String
generateEvolution 0 xs = xs
generateEvolution n xs | elem '#' (take 3 xs) = error ("Enlarge prefix! (" ++ show n ++ ")")
                       | otherwise            = generateEvolution (n-1) $ evolution xs

prefix :: String
prefix = "....."

postfix :: String
postfix = "....."

puzzle_12_1 :: Int
puzzle_12_1 = sum $ map fst $ filter (\(_,p) -> p == '#') $ zip [(-(length prefix))..] (generateEvolution 20 (prefix ++ initialState ++ postfix))

-- Too slow
puzzle_12_2 :: Int
puzzle_12_2 = sum $ map fst $ filter (\(_,p) -> p == '#') $ zip [(-(length prefix))..] (generateEvolution 50000000000 (prefix ++ initialState ++ postfix))

main = putStrLn (show puzzle_12_2)

