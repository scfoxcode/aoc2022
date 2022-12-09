module Main where
import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Debug.Trace
-- Okay. Building a tree like this is impossible in haskell
-- Look at zippers, learnyouahaskell

-- From a list of command input and output
-- we need to build a tree of the filesystem
-- once we have this, the rest should be easy
data NodeType = Directory | File
    deriving (Eq, Show)
data Tree = Tree {
    nodeType :: NodeType,
    children :: [Tree],
    parent :: Maybe Tree,
    name :: String,
    size :: Int
} deriving (Show)
data Command = CD_D String | CD_UP | CD_ROOT | LS | NOP

addChild :: Tree -> Tree -> Tree
addChild tree child
    | trace("Add child " ++ (name child) ++ (show $ length (children tree))) False=undefined
addChild tree child =
    if exists then
        tree else do
        -- this must be the bug
        -- I fucked bet Just tree is a totally different instance to tree
        let updatedChild = child { parent = Just updatedParent }
        let updatedParent = tree { children = (children tree) ++ [updatedChild] }
        updatedParent
        -- tree { children = (children tree) ++ [child { parent = Just tree }] } 
    where exists = length (filter (\n -> (name child) == (name n)) (children tree)) > 0

rootDir :: Tree -> Tree
rootDir tree
    | trace ("Root dir " ++ (name tree) ++ (show $ length (children tree))) False = undefined
rootDir tree =
    case (parent tree) of
        Just a -> rootDir a
        Nothing -> tree

upOneDir :: Tree -> Tree
upOneDir tree =
    case (parent tree) of
        Just a -> a
        Nothing -> tree

changeDir :: String -> Tree -> Tree
changeDir dir tree 
    | trace ("Testing the func " ++ (name $ head dirs) ++ (show $ length dirs)) False = undefined
    | length dirs == 0 = tree
    | otherwise = head dirs 
    where dirs = filter (\c -> (name c) == dir && (nodeType c) == Directory) (children tree)

lineToCommand :: [String] -> Command
lineToCommand parts 
    | parts !! 1 == "cd" && (parts !! 2) == "/" = CD_ROOT
    | parts !! 1 == "cd" && (parts !! 2) == ".." = CD_UP
    | parts !! 1 == "cd" = (CD_D val) 
    | parts !! 1 == "ls" = LS 
    | otherwise = NOP
    where val = parts !! 2

executeCommand :: Command -> Tree -> Tree
executeCommand NOP tree = tree
executeCommand CD_ROOT tree = rootDir tree 
executeCommand CD_UP tree = upOneDir tree
executeCommand (CD_D dir) tree = changeDir dir tree
executeCommand LS tree = tree -- TODO do we care about this command

buildTree :: Tree -> [String] -> Tree
buildTree tree lines
    | length lines == 0 = rootDir tree
    | (parts !! 0) == "$" = step (executeCommand (lineToCommand parts) tree)
    | (parts !! 0) == "dir" = step (addChild tree $ Tree Directory [] Nothing (parts !! 1) 0)
    -- we are in the size of file case
    -- Nothing here is weird, will we always know the parent? can change this
    | otherwise = step (addChild tree $ Tree File [] Nothing (parts !! 1) (read $ parts !! 0))
    where
        parts = words $ head lines 
        step = \a -> buildTree a (tail lines)

sizeOfDir:: Tree -> Int
sizeOfDir tree = 
    foldl
    (\acc t ->
        if (nodeType t) == Directory then
            (acc + sizeOfDir t) else
            (acc + size t))
    0 childNodes 
    where childNodes = children tree

findSizes :: Tree -> [Int]
findSizes tree = do
    let size = sizeOfDir tree 
    let dirs = filter (\n -> nodeType n == Directory) $ children tree
    -- >>= reminder that it's basically flatmap
    [size] ++ (dirs >>= findSizes) 
    
main = do
    file <- readFile "input2.txt"
    let filesystem = buildTree (Tree Directory [] Nothing "/" 0) (lines file)
    -- let ans = foldl (\acc s -> if s <= 100000 then acc + s else acc) 0 $ findSizes filesystem
    putStrLn $ name filesystem
    putStrLn $ show $ length (children filesystem)
    -- putStrLn $ name $ (children filesystem) !! 0
    -- putStrLn $ name $ (children filesystem) !! 1
    -- putStrLn $ show $ length (findSizes filesystem)
    -- putStrLn $ show $ length $ filter (\a -> (name a) == "blgtdv") (children filesystem)
    -- putStrLn $ show filesystem 
    putStrLn "Hello"  
    -- putStrLn $ "This won't be right: " ++ show ans

