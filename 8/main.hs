module Main where
import System.IO
import Control.Monad
import Data.List
import Data.Char

visibleFromAbove :: [String] -> Int -> Int -> Bool
visibleFromAbove _ _ 0 = True
visibleFromAbove grid x y = length (filter testPos (reverse [0..(y-1)])) == 0
    where
        height = treeHeight grid x y   
        testPos = \row -> treeHeight grid x row >= height

visibleFromBelow :: [String] -> Int -> Int -> Bool
visibleFromBelow grid x y
    | y == gridMaxRow = True
    | otherwise = length (filter testPos [(y+1)..gridMaxRow]) == 0
    where
        height = treeHeight grid x y   
        testPos = \row -> treeHeight grid x row >= height
        gridMaxRow = (length grid) - 1

visibleFromLeft :: [String] -> Int -> Int -> Bool
visibleFromLeft _ 0 _ = True
visibleFromLeft grid x y = length (filter testPos (reverse [0..(x-1)])) == 0
    where
        height = treeHeight grid x y   
        testPos = \col -> treeHeight grid col y >= height

visibleFromRight :: [String] -> Int -> Int -> Bool
visibleFromRight grid x y
    | x == gridMaxCol = True
    | otherwise = length (filter testPos [(x+1)..gridMaxCol]) == 0
    where
        height = treeHeight grid x y   
        testPos = \col -> treeHeight grid col y >= height 
        gridMaxCol = (length $ head grid) - 1

visibleFromAny :: [String] -> Int -> Int -> Bool
visibleFromAny grid x y =
    visibleFromAbove grid x y || 
    visibleFromBelow grid x y || 
    visibleFromLeft grid x y || 
    visibleFromRight grid x y

treeHeight :: [String] -> Int -> Int -> Int 
treeHeight grid x y = digitToInt $ (grid !! y) !! x

main = do
    content <- readFile "input.txt"
    let grid = lines content
    let height = (length grid) - 1
    let width = (length $ head grid) - 1
    let visible = [visibleFromAny grid x y | x <- [0..width], y <- [0..height]]
    let numvisible = length $ filter (\a -> a == True) visible
    putStrLn ("Num trees " ++ (show ((width + 1) * (height + 1))))
    putStrLn ("Total visible: " ++ (show $ length visible))
    putStrLn ("Number visible: " ++ (show numvisible))

