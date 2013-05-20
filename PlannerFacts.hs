module PlannerFacts where

import DataStructureFacts
import Facts
import Data.List.Split
import System.Environment   
import FactsParser

plan :: World -> Fluent -> Plan
plan w1 fluent = planner w1 fluent [] [[(w1,[])]] []

-- Returns the number of columns in the world
--cols :: World -> Int
--cols world = length blocks
--  where
--    (World (blocks, _)) = world

-- Set the number of columns in the world
cols = 10

--PLANNER
planner :: World -> Fluent -> [World] -> [[(World, Plan)]] -> [[(World, Plan)]] -> Plan
planner w1 fluent acc (((w, plan):xs):xss) stack1
  | satisfies w fluent = reverse plan
  | w `elem` acc  = planner w fluent acc (xs:xss) stack1 
  | w1 `elem` acc = planner w fluent acc (xs:xss) stack1 
  | otherwise     = planner w fluent (w:acc) (xs:xss) (build_plan w plan:stack1)
planner w1 fluent acc ([]:xss) stack1 = planner w1 fluent acc xss stack1 
planner w1 fluent acc [] stack1 = planner w1 fluent acc (reverse stack1) []

build_plan'' :: World -> Plan -> [(World, Plan)]
build_plan'' w p =  filter check_actions list
  where
    list = build_plan w p

-- Build all legal plans for a specific world and the resulting
-- worlds from applying those plans
build_plan :: World -> Plan -> [(World, Plan)]
build_plan world plan = build_plan' world plan [] possible_actions
 where
   build_plan' world plan acc []  = acc 
   build_plan' world plan acc (action:actions)
     | is_allowed = build_plan' world plan ((new_world, action:plan):acc) actions
     | otherwise  = build_plan' world plan acc actions
     where
       (new_world, is_allowed) = execute world action

-- Returns all possible actions
possible_actions :: Plan
possible_actions = [(y,x)| x <- [0..cols-1], y <- ["pick", "drop"]]

-- Executes an action on a world and returns the resulting
-- world and along with the validity
execute :: World -> (String, Int) -> (World, Bool)
execute world ("pick", column) = pick world column
execute world ("drop", column) = dropp world column

-- Picks up a block form a specific column (if allowed)
pick (World (world, grabber)) column
  | null curr_column = (World (world, grabber), False)
  | grabber == Clear = (World (new_world, Grabber new_grabber), True)
  | otherwise        = (World (world, grabber), False)
  where
    new_world   = (world !!= (column, new_column)) -- make first elem of column empty
    new_grabber = head curr_column -- get the block to pick 
    new_column  = tail curr_column -- remove block from column
    curr_column = get_stack world column

-- Drops a block on top of a column (if allowed)
dropp :: World -> Int -> (World, Bool)
dropp (World (world, block)) column
  | block == Clear = (World (world, block), False) -- Nothing to drop, exep?
  | otherwise  = (World (new_world, Clear), True)
   where     
     new_world   = world !!= (column, new_column)
     new_column  = [(grabber_to_block block)] ++ curr_column 
     curr_column = get_stack world column -- column to drop on

-- Extracts a block from a grabber
grabber_to_block :: Grabber -> Block
grabber_to_block (Grabber (Block name x y z)) = Block name x y z
grabber_to_block _ = (error "something went wrong :(")


-- Updates the ith element in a list with v    
(!!=) :: [a] -> (Int ,a) -> [a]
(x:xs) !!= (0,el) = (el:xs)
(x:xs) !!= (n,el) = (x:( xs !!= (n-1,el)))

-- Returns a specific column from a world                    
get_stack :: [[Block]] -> Int -> [Block]
get_stack blocks col = blocks !! col

-- Wrapper function for is_valid_method
check_actions :: (World, Plan) -> Bool
check_actions (world,_) = is_Valid world

-- Checks whether a world is valid or not
is_Valid :: World -> Bool
is_Valid world = and rules
 where
   (World (blocks, grabber)) = world
   rules = [shape_not_top world Pyramid,
            shape_not_top world Ball]
-- Rules --
-- Pyramids and balls cannot support anything (gen function)
shape_not_top :: World -> Shape -> Bool
shape_not_top world shape = not $ or check_shape_pos
 where
   (World (blocks, _)) = world
   check_shape_pos     = map pos_shape ignore_first_blocks 
   pos_shape           = any (`is_shape` shape)
   ignore_first_blocks = map tail (filter (not . null) blocks)

-- Checks if a world has any blocks supported
-- by smaller blocks
check_size :: World -> Bool
check_size world = and sizes 
   where
     (World (blocks, _)) = world
     sizes = run (zip pairs_lists blocks)
     pairs_lists = [[(x,x+1) | x <- [0 .. (length stack)-2] ] |  stack <- blocks] 

-- Helper function for check_size that compares
-- two blocks in a column
compare_blocks :: [Block] -> (Int,Int) -> Bool
compare_blocks stack (pos1, pos2) = cmp y yy
  where
    Block name1 x  y  z  = stack !! pos1
    Block name2 xx yy zz = stack !! pos2


run :: [([(Int,Int)],[Block])] -> [Bool]
run [] = []
run ((pos,stack):xs) = (map (compare_blocks stack) pos)++run xs
    
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

-- HELPER -- Check if a block is of a specific shape
is_shape :: Block -> Shape -> Bool
is_shape (Block _ x _ _) shape
  | x == shape = True
  | otherwise  = False


main :: IO ()
main = do
       args <- getArgs
       print (init $ parse_plan $ run_parse args)


run_parse :: [String] -> Plan
run_parse args =  plan start_world fluent
 where
   w_str      = args !! 0
   fluent_str = args !! 1
   blks_map   = args !! 2
   (start_world, fluent) = create_world w_str fluent_str blks_map


parse_plan :: Plan -> String
parse_plan [] = []
parse_plan ((action,col):xs) = (action++" "++(show col))++";"++parse_plan xs
