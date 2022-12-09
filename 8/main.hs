module Main where
import System.IO
import Control.Monad
import Data.List
import Data.Char

treeHeight :: [String] -> Int -> Int -> Int 
treeHeight grid x y = digitToInt $ (grid !! y) !! x

-- Part 1
visibleFromX :: [String] -> (Int -> Bool) -> [Int] -> Bool
visibleFromX grid testFn vals = length (filter testFn vals) == 0

visibleFromAny :: [String] -> Int -> Int -> Bool
visibleFromAny grid x y =
    visibleFromX grid horizontal (reverse [0..(x-1)]) || -- left
    visibleFromX grid horizontal [(x+1)..gridMaxCol]  || -- right 
    visibleFromX grid vertical (reverse [0..(y-1)])   || -- above 
    visibleFromX grid vertical [(y+1)..gridMaxRow]       -- below
    where
        height = treeHeight grid x y   
        gridMaxCol = (length $ head grid) - 1
        gridMaxRow = (length grid) - 1
        vertical = \row -> treeHeight grid x row >= height
        horizontal = \col -> treeHeight grid col y >= height  

-- Part 2

-- dont think I trust find index like this
counter :: Int -> [Int] -> In-- t

treesInDir :: [String] -> (Int -> Bool) -> [Int] -> Int 
treesInDir grid testFn vals =
    case ans of
        Just a -> a + 1
        Nothing -> 0 
    where
        ans = findIndex (== True) (map testFn vals) 

scenicScore :: [String] -> Int -> Int -> Int 
scenicScore grid x y
    | x == 0 = 0
    | y == 0 = 0
    | x == (length $ (head grid)) - 1 = 0
    | y == (length grid) - 1 = 0
    | otherwise = 
    (treesInDir grid horizontal (reverse [0..(x-1)])) * -- left
    (treesInDir grid horizontal [(x+1)..gridMaxCol]) *  -- right 
    (treesInDir grid vertical (reverse [0..(y-1)])) *   -- above 
    (treesInDir grid vertical [(y+1)..gridMaxRow])      -- below
    where -- would be nice to avoid this copy paste
        height = treeHeight grid x y   
        gridMaxCol = (length $ head grid) - 1
        gridMaxRow = (length grid) - 1
        vertical = \row -> treeHeight grid x row >= height
        horizontal = \col -> treeHeight grid col y >= height  

maxNum :: Ord a => [a] -> a
maxNum [a] = a
maxNum (a:a':as) = maxNum ((if a >= a' then a else a'):as)

main = do
    content <- readFile "input.txt"
    let grid = lines content
    let height = (length grid) - 1
    let width = (length $ head grid) - 1
    let visible = [visibleFromAny grid x y | x <- [0..width], y <- [0..height]]
    let sceneScores = [scenicScore grid x y | x <- [0..width], y <- [0..height]]
    let numVisible = length $ filter (\a -> a == True) visible
    putStrLn ("Num trees " ++ (show ((width + 1) * (height + 1))))
    putStrLn ("Number visible: " ++ (show numVisible))
    putStrLn ("Highest Scenic Score: " ++ (show $ maxNum sceneScores))

