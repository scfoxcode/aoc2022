module Main where
import System.IO
import Control.Monad
import Data.List
import Data.Char

visibleFromX :: [String] -> (Int -> Bool) -> [Int] -> Bool
visibleFromX grid testFn vals = length (filter testFn vals) == 0

visibleFromAny :: [String] -> Int -> Int -> Bool
visibleFromAny grid x y =
    visibleFromX grid horizontal (reverse [0..(x-1)]) || --left
    visibleFromX grid horizontal [(x+1)..gridMaxCol]  || -- right 
    visibleFromX grid vertical (reverse [0..(y-1)])   || -- above 
    visibleFromX grid vertical [(y+1)..gridMaxRow]       -- below
    where
        gridMaxCol = (length $ head grid) - 1
        gridMaxRow = (length grid) - 1
        height = treeHeight grid x y   
        vertical = \row -> treeHeight grid x row >= height
        horizontal = \col -> treeHeight grid col y >= height  

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
    putStrLn ("Number visible: " ++ (show numvisible))

