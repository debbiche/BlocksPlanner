module FactsParser where

import Data.List.Split
import DataStructureFacts
import Facts
import Data.List

-- Test the parse functions

test = (testWorld,testFluent) == test_parse

test_parse :: (World, Fluent)
test_parse = create_world w1 fluent maps
   where
     w1 = ";a b;c d;;e f g h i;;;j k;;l m;grabber empty"
     fluent = "(ontop b m)"
     maps = "a rectangle tall blue;"++
            "b ball large white;"++
            "c square large red;"++
            "d pyramid large green;"++
            "e box large white;"++
            "f rectangle wide black;"++
            "g rectangle wide blue;"++
            "h rectangle wide red;"++
            "i pyramid small yellow;"++
            "j box large red;"++
            "k ball small yellow;"++
            "l box medium red;"++
            "m ball medium blue"


-- Main method that builds the start and end worlds objects
create_world :: String -> String -> String -> (World,Fluent)
create_world w1 f_str maps = (world,fluent)
   where
     blocks_map   = create_blocks maps -- get block names
     split_colsw1 = map (splitOn " ") (init $ splitOn ";" w1)
     blocks_w1    = create_columns split_colsw1 blocks_map
     grabber_w1   = create_grabber grabber_str1 blocks_map
     world        = World (blocks_w1, grabber_w1)
     grabber_str1 = last $ splitOn " " (last $ splitOn ";" w1)
     fluent       = extract_fluent f_str blocks_map 


-- Create the grabber object for a world
create_grabber :: String -> [(String,Block)] -> Grabber
create_grabber "empty" maps = Clear
create_grabber str maps     = Grabber block 
  where
     Just block = lookup str maps


-- Creates the columns of a world 
create_columns :: [[String]] -> [(String,Block)] -> [[Block]]
create_columns [] _ = []
create_columns ([]:xs) blocks = [] ++ create_columns xs blocks
create_columns (x:xs)    blocks = [(create_column x blocks)]++
                                  (create_columns xs blocks)

    
-- Creates a single column of blocks
create_column :: [String] -> [(String,Block)]-> [Block]
create_column [] _  = [] 
create_column (x:xs)  maps 
 | null x = []
 | otherwise =  reverse $ block:create_column xs maps
  where
    Just block = lookup x maps

-- Create actual block objects from a string 
create_blocks :: String -> [(String,Block)]
create_blocks str = map extract_block blocks
  where
    blocks = splitOn ";" str

-- Takes a string with a name and block elements
-- and returns a tuple with name and block
extract_block :: [Char] -> (String, Block)
extract_block str  = (name, Block name shape size color)
  where
    name  = blocks !! 0
    shape = parse_shape $ blocks !! 1
    size  = parse_size  $ blocks !! 2
    color = parse_color $ blocks !! 3
    blocks = splitOn " " str

extract_fluent :: String -> [(String,Block)] -> Fluent
extract_fluent str block_maps
   | isInfixOf "&" str = And $ parse_fluent "&" str block_maps
   | isInfixOf "|" str = Or $ parse_fluent "|" str block_maps
   | otherwise         = parse_fact str block_maps 

parse_fluent :: String -> String -> [(String,Block)] -> [Fluent]
parse_fluent sep fluent blocks_map = list 
   where
     facts = splitOn sep fluent
     list = [parse_fact fact blocks_map | fact <- facts]

parse_fact :: String -> [(String,Block)] -> Fluent
parse_fact str block_maps
 | isInfixOf "grabber" str = get_in_grabber str block_maps
 | otherwise = Fluent f
 where
   split = splitOn " " str
   type_str = tail (split !! 0) --rem the ( in 1st elem
   b1_str = split !! 1
   b2_str = init (split !! ((length split)-1))
   f = get_fact type_str b1 b2 
   Just b1 = lookup b1_str block_maps
   Just b2 = lookup b2_str block_maps

get_in_grabber :: String -> [(String,Block)] -> Fluent
get_in_grabber str block_maps = Fluent f
  where
   split = splitOn " " str
   b1_str = init (split !! 1) --rem the ( in 1st elem
   f = InGrabber b1 True
   Just b1 = lookup b1_str block_maps
   

get_fact :: [Char] -> Block -> Block -> Fact
get_fact "ontop" b1 b2     = OnTop b1 b2 True
get_fact "leftof" b1 b2    = LeftOf b1 b2 True
get_fact "rightof" b1 b2   = RightOf b1 b2 True
get_fact "above" b1 b2     = Above b1 b2 True
get_fact "under" b1 b2     = Under b1 b2 True
--get_fact "iscolumn" = IsColumn a True
--get_fact "isblock" = IsBlock a True

-- Map a shape string to shape object
parse_shape :: String -> Shape
parse_shape "square"    = Square
parse_shape "rectangle" = Rectangle
parse_shape "pyramid"   = Pyramid
parse_shape "ball"      = Ball
parse_shape "box"       = Box

-- Map a color string to a color object
parse_color :: String -> Color
parse_color "red"    = Red
parse_color "black"  = Black
parse_color "blue"   = Blue
parse_color "green"  = Green
parse_color "yellow" = Yellow
parse_color "white"  = White

-- Map a string size to a size object
parse_size :: String -> Size
parse_size "large"  = Large
parse_size "medium" = Medium
parse_size "small"  = Small
parse_size "tall"   = Tall   
parse_size "wide"   = Wide   
