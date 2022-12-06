module Main where
import System.IO
import Control.Monad
import Data.List

substring :: Int -> Int -> String -> String
substring markerSize i inp = take markerSize (drop i inp)

hasDuplicates :: String -> Bool
hasDuplicates s = length (nub s) /= length s 

findMarkerIndex :: Int -> Int -> String -> Int
findMarkerIndex markerSize count inp = 
    if hasDuplicates $ substring markerSize count inp
        then findMarkerIndex markerSize (count + 1) inp
    else count + markerSize 

main = do
    file <- openFile "input.txt" ReadMode
    contents <- hGetContents file
    putStrLn  $ "Part 1: " ++ (show $ findMarkerIndex 4 0 contents)
    putStrLn  $ "Part 2: " ++ (show $ findMarkerIndex 14 0 contents)
