module Planner where

import Data.List.Split
import System.Environment   
import Parser
import DataStructure

--SHAPES CREATION
a = Block Rectangle Tall Blue
b = Block Ball Large White
c = Block Square Large Red
d = Block Pyramid Large Green
e = Block Box Large White
f = Block Rectangle Wide Black
g = Block Rectangle Wide Blue
h = Block Rectangle Wide Red
i = Block Pyramid Small Yellow
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
s1 = [b,a]
s2 = [d,c]
s3 = [i,h,g,f,e]
s4 = [k,j]
s5 = [m,l]

start_world :: World
--start_world = World ([empty,s1,s2,empty,s3,empty,empty,s4,empty,s5],Clear)
start_world = World ([s1,s2,empty,empty,empty,empty,empty,empty,empty,empty],Clear)

-- Returns the number of columns in the world
cols :: Int
cols = length blocks
  where
    (World (blocks, _)) = start_world

--FINAL WORLD CREATION
s11 = [b,a]
s22 = [b,d,c]
s33 = [i,h,g]
s44 = [k,j]
s55 = [m,l]

end_world :: World
--end_world = World ([empty,s11,s22,empty,s33,empty,empty,s44,empty,s55],Clear)
end_world = World ([s11,s22,empty,empty,empty,empty,empty,empty,empty,empty],Clear)

cc :: World -> World -> Bool
cc w1 w2 = b1 == b2
  where
    (World (b1, _)) = w1
    (World (b2, _)) = w2

--PLANNER
planner :: World -> World -> [World] -> [[(World, Plan)]] -> [[(World, Plan)]] -> Plan
planner w1 w2 acc (((w, plan):xs):xss) stack1
  | w == w2      = reverse plan -- base case
  | w1 `elem` acc = planner w w2 acc (xs:xss) stack1 -- already visited this world
  | w `elem` acc = planner w w2 acc (xs:xss) stack1 -- already visited this world
  | otherwise    = planner w w2 (w:acc) (xs:xss) ((build_plan w plan):stack1)
planner w1 w2 acc ([]:xss) stack1 = planner w1 w2 acc xss stack1 -- No brothers 
planner w1 w2 acc [] stack1 = planner w1 w2 acc (reverse stack1) []

build_plan'' :: World -> Plan -> [(World, Plan)]
build_plan'' w p =  filter check_actions list
  where
    list = build_plan w p

build_plan :: World -> Plan -> [(World, Plan)]
build_plan  world plan = build_plan' world plan [] possible_action
 where
   build_plan' world plan acc []  = acc 
   build_plan' world plan acc (action:actions)
     | is_allowed = build_plan' world plan ((new_world, (action:plan)):acc) actions
     | otherwise  = build_plan' world plan acc actions
     where
       (new_world, is_allowed) = execute world action

possible_action :: Plan
possible_action =  [(y,x)| x <- [0..(cols-1)], y <- ["pick", "drop"]]

execute :: World -> (String, Int) -> (World, Bool)
execute world ("pick", column) = pick world column
execute world ("drop", column) = dropp world column



test :: World -> Int -> Bool
test w col = and exist
  where
    curr_stack = blocks !! col
    end_stack = end_blocks !! col
    (World (blocks, grabber)) = w
    (World (end_blocks, _)) = end_world
    exist = [block `elem` end_stack | block <- curr_stack]
    





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
  | block /= Clear  = (World (new_world, Clear), True)
  | otherwise = (World (world, block), False)
   where     
     new_world   = world !!= (column, new_column)
     new_column  = [(grabber_to_block block)] ++ curr_column 
     curr_column = get_stack world column -- column to drop on
     tomas = test (World (world, block)) column

grabber_to_block :: Grabber -> Block
grabber_to_block (Grabber (Block x y z)) = Block x y z
grabber_to_block _ = (error "something went wrong :(")


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

check_actions :: (World, Plan) -> Bool
check_actions (world,_) = is_Valid world


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


-- not tested yet
check_size :: World -> Bool
check_size world = and sizes 
   where
     (World (blocks, _)) = world
     sizes = run (zip pairs_lists blocks)
     pairs_lists = [[(x,x+1) | x <- [0 .. (length stack)-2] ] |  stack <- blocks] 


compare_blocks :: [Block] -> (Int,Int) -> Bool
compare_blocks stack (pos1, pos2) = cmp y yy
  where
    Block x  y  z  = stack !! pos1
    Block xx yy zz = stack !! pos2


run :: [([(Int,Int)],[Block])] -> [Bool]
run [] = []
run ((pos,stack):xs) = (map (compare_blocks stack) pos)++run xs


aa = [[(x,x+1) | x <- [0 .. (length stack)-2] ] |  stack <- [empty,s11,s22,empty,s33,empty,empty,s44,empty,s55]]

bb = [empty,s11,s22,empty,s33,empty,empty,s44,empty,s55]
    
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


--main :: (World,World)
main = do
       args <- getArgs
       putStrLn (show $ run_parse args)


run_parse :: [String] -> (World, World)
run_parse args =  create_world x xs xss
 where
   x = args !! 0
   xs = args !! 1
   xss = args !! 2
