module Parser where

import Data.List.Split
import DataStructure

-- Test the parse functions
test_parse :: (World, World)
test_parse = create_world w1 w2 maps
  where
    w1 = "a b;c d;;e f g h i;;;j k;;l m;grabber empty"
    w2 = "a b;c d;;e f g h i;;;j k;;l m;grabber empty"
    maps = "a ball small red;"++
           "b ball small blue;"++
           "c ball small green;"++
           "d ball small white;"++
           "e ball small red;"++
           "f ball small red;"++
           "g ball small red;"++
           "h ball small red;"++
           "i ball small red;"++
           "j ball small red;"++
           "k ball small red;"++
           "l ball small red;"++
           "m ball small red"

-- Main method that builds the start and end worlds objects
create_world :: String -> String -> String -> (World,World)
create_world w1 w2 maps = (world1,world2)
  where
    blocks_map   = create_blocks maps -- get block names
    split_colsw1 = map (splitOn " ") (init $ splitOn ";" w1)
    split_colsw2 = map (splitOn " ") (init $ splitOn ";" w2)
    blocks_w1    = create_columns split_colsw1 blocks_map
    blocks_w2    = create_columns split_colsw2 blocks_map
    grabber_w1   = create_grabber grabber_str1 blocks_map
    grabber_w2   = create_grabber grabber_str2 blocks_map
    world1       = World (blocks_w1, grabber_w1)
    world2       = World (blocks_w2, grabber_w2)
    grabber_str1 =  last $ splitOn " " (last $ splitOn ";" w1)
    grabber_str2 =  last $ splitOn " " (last $ splitOn ";" w2)


-- Create the grabber object for a world
create_grabber :: String -> [(String,Block)] -> Grabber
create_grabber "empty" maps = Clear
create_grabber str maps     = Grabber block 
  where
     Just block = lookup str maps


-- Creates the columns of a world 
create_columns :: [[String]] -> [(String,Block)] -> [[Block]]
create_columns [] _ = []
create_columns ([""]:xs) blocks = [] ++ create_columns xs blocks
create_columns (x:xs)    blocks = [(create_column x blocks)]++
                                  (create_columns xs blocks)
    
-- Creates a single column of blocks
create_column :: [String] -> [(String,Block)]-> [Block]
create_column [] _  = []
create_column (x:xs)  maps = block:create_column xs maps
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
extract_block str  = (name, Block shape size color)
  where
    name  = blocks !! 0
    shape = parse_shape $ blocks !! 1
    size  = parse_size  $ blocks !! 2
    color = parse_color $ blocks !! 3
    blocks = splitOn " " str 

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
