module World where

import Data.List
import Data.Maybe
import Data.String

data Shape = Square | Rectangle | Pyramid |
             Ball | Box deriving (Show, Eq)
                    
data Color = Red | Black | Blue |
             Green | Yellow | White deriving (Show, Eq)

data Size = Large | Medium |
            Small | Tall | Wide deriving (Show, Eq)
                   
data Block = Block Shape Size Color deriving (Show, Eq)

data Grabber = Clear | Grabber Block deriving (Show, Eq)

--data Stack   = Empty | Stack [Block] deriving (Show, Eq)
                    
newtype World = World ([[Block]], Grabber) deriving (Show, Eq)
--data World = World ([Stack], Grabber) deriving (Show, Eq)

type Plan = [(String, Int)]

-- (String, Int) = action

--SHAPES CREATION
a = Block Rectangle Tall Blue
b = Block Ball Small White
c = Block Square Large Red
d = Block Pyramid Large Green
e = Block Box Large White
f = Block Rectangle Wide Black
g = Block Rectangle Wide Blue
h = Block Rectangle Wide Red
i = Block Pyramid Medium Yellow
j = Block Box Large Red
k = Block Ball Small Yellow
l = Block Box Medium Red
m = Block Ball Medium Blue


plan :: World -> World -> Plan
plan w1 w2 = planner w1 w2 [] [[(w1,[])]] []
  
    

 --INITIAL WORLD CREATION
empty :: [Block]    
empty = []

-- Reverse is needed for now to reflect design
s1 = reverse [a,b]
s2 = reverse [c,d]
s3 = reverse [e,f]
s4 = reverse [j,k]
s5 = reverse [l,m]

start_world :: World
--start_world = World ([empty,s1,s2,empty,s3,empty,empty,s4,empty,s5],Clear)
start_world = World ([empty,s1,s2],Clear)

-- Returns the number of columns in the world
cols :: Int
cols = length blocks
  where
    (World (blocks, _)) = start_world

--FINAL WORLD CREATION
s11 = reverse [a,c,d]
s22 = reverse [b]
s33 = reverse [e,f]
s44 = reverse [j,k]
s55 = reverse [l,m]

end_world :: World
--end_world = World ([empty,s11,s22,empty,s33,empty,empty,s44,empty,s55],Clear)
end_world = World ([s11,empty,s22],Clear)

cc :: World -> World -> Bool
cc w1 w2 = b1 == b2
  where
    (World (b1, _)) = w1
    (World (b2, _)) = w2

--PLANNER
planner :: World -> World -> [World] -> [[(World, Plan)]] -> [[(World, Plan)]] -> Plan
--planner world1 world2 acc [] c = (error "Planner called with empty lists")
planner w1 w2 acc (((w, plan):xs):xss) stack1
  | (cc w w2) == True  = reverse plan -- base case
  | w `elem` acc     = planner w1 w2 acc (xs:xss) stack1 -- already visited this world
  | otherwise        = planner w w2 (w:acc) (xs:xss) ((build_plan w plan):stack1)
planner w1 w2 acc ([]:xss) stack1 = planner w1 w2 acc xss stack1 -- No brothers 
planner w1 w2 acc [] stack1 = planner w1 w2 acc (reverse stack1) []


build_plan :: World -> Plan -> [(World, Plan)]
build_plan  world plan = build_plan' world plan [] possible_action
 where
   build_plan' world plan acc []  = acc
   build_plan' world plan acc (action:actions)
     | is_allowed = build_plan' world plan ((new_world, (action:plan)):acc) actions
     | otherwise  = build_plan' world plan acc actions
     where
       (new_world, is_allowed) = execute world action
       allowed = is_Valid new_world
--       valid_actions = 

possible_action :: Plan
possible_action =  [(y,x)| x <- [0..(cols-1)], y <- ["pick", "drop"]]

execute :: World -> (String, Int) -> (World, Bool)
execute world ("pick", column) = pick world column
execute world ("drop", column) = dropp world column

pick :: World -> Int -> (World, Bool)
pick (World (world, grabber)) column
  | null curr_column = (World (world, grabber), False)
  | grabber == Clear = (World (new_world, Grabber new_grabber), True)
  | otherwise        = (World (world, grabber), False)
  where
    new_world   = (world !!= (column, new_column)) -- make first elem of column empty
    new_grabber = head curr_column -- get the block to pick 
    new_column  = tail curr_column -- remove block from column
    curr_column = get_stack world column

dropp :: World -> Int -> (World, Bool)
dropp (World (world, block)) column
  | block == Clear = (World (world, block), False) -- Nothing to drop, exep?
  | otherwise      = (World (new_world, Clear), True)     
   where     
     new_world      = world !!= (column, new_column)
     new_column     = [(grabber_to_block block)] ++ curr_column 
     curr_column    = get_stack world column -- column to drop on

grabber_to_block :: Grabber -> Block
grabber_to_block (Grabber (Block x y z)) = Block x y z


-- Updates the ith element in a list with v    
(!!=) :: [a] -> (Int ,a) -> [a]
(x:xs) !!= (0,el) = (el:xs)
(x:xs) !!= (n,el) = (x:( xs !!= (n-1,el)))

                    
get_stack :: [[Block]] -> Int -> [Block]
get_stack blocks col = blocks !! col


get_blocks :: World -> [[Block]]
get_blocks (World (blocks, _)) = blocks

-- for testing purposes
picked :: World
picked = world
  where
    (world, _) = pick start_world 1

check_actions :: [(World, Plan)] -> [Bool]
check_actions [] = []
check_actions ((world,plan):xs) = [is_Valid world] ++ check_actions xs


-- Checks whether a world is valid or not
is_Valid :: World -> Bool
is_Valid world = and rules
 where
   (World (blocks, grabber)) = world
   rules = [shape_not_top world Pyramid,
            shape_not_top world Ball]
-- Rules
--Pyramids and balls cannot support anything (gen function)
shape_not_top :: World -> Shape -> Bool
shape_not_top world shape = not $ or check_shape_pos
 where
   (World (blocks, _)) = world
   check_shape_pos     = map pos_shape ignore_first_blocks 
   pos_shape column    = any (\x -> is_shape x shape) column
   ignore_first_blocks = map tail (filter (not . null) blocks) -- disrecard 1st pos

--Size = Large | Medium | Small | Tall | Wide

-- not tested yet
-- check_size :: World -> Bool
-- check_size world = and [and x | x <- sizes] 
--   where
--     (World (blocks, _)) = world
--     sizes =  [[map (compare_blocks stack) pos| pos <- pairs_lists] | stack <- blocks]
--     pairs_lists =  [[(x,x+1) | x <- [0 .. (length stack)-2] ] |  stack <- blocks]-- rem []s


compare_blocks :: [Block] -> (Int,Int) -> Bool
compare_blocks stack (pos1, pos2) = cmp y yy
  where
    Block x  y  z  = stack !! pos1
    Block xx yy zz = stack !! pos2
    
    
--Very naive compare method
cmp :: Size -> Size -> Bool
cmp s1 s2
 | s1 == s2       = True
cmp Large  _      = True
cmp _      Large  = False
cmp Medium _      = True
cmp _      Medium = False 
cmp Small  _      = True
cmp _      Small  = False
cmp Wide   _      = True
cmp _      Wide   = False
cmp Tall   _      = True
--cmp _      Tall   = False

-- HELPER -- Check if a block is of a specific shape
is_shape :: Block -> Shape -> Bool
is_shape (Block x _ _) shape
  | x == shape = True
  | otherwise  = False


