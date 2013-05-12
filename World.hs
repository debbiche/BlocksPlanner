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

type Plan = [(String, Int)] -- consider changing to actio

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


 --INITIAL WORLD CREATION
empty :: [Block]    
empty = []

-- Reverse is needed for now to reflect design
s1 = reverse [a,b]
s2 = reverse [c,d]
s3 = reverse [e,f,g,h,i]
s4 = reverse [j,k]
s5 = reverse [l,m]

initial_world :: World
initial_world = World ([empty,s1,s2,empty,s3,empty,empty,s4,empty,s5],Clear)

--FINAL WORLD CREATION
s11 = reverse [a,b,c]
s22 = reverse [b,d]
s33 = reverse [e,f,h,i]
s44 = reverse [g,j,k]
s55 = reverse [m,l]

final_world :: World
final_world = World ([empty,s11,s22,empty,s33,empty,empty,s44,empty,s55],Clear)

--PLANNER
planner :: World -> World -> [World] -> [[(World, Plan)]] -> [(World, Plan)] -> Plan
planner world1 world2 acc [] [] = (error "Planner called with empty lists")
planner world1 world2 acc (((w, plan):xs):xss) stack1
  | world1 == world2 = reverse plan
  | w `elem` acc     = planner world1 world2 acc (xs:xss) stack1 
  | otherwise        = planner w world2 (w:acc) (xs:xss) ((build_plan w plan)++stack1)
planner world1 world2 acc ([]:xss) stack1 = planner world1 world2 acc xss stack1
planner world1 world2 acc [] stack1 = planner world1 world2 acc [] (reverse stack1)

build_plan :: World -> Plan -> [(World, Plan)]
build_plan  world plan = build_plan' world plan [] possible_action

build_plan' :: World -> Plan -> [(World, Plan)]
               -> Plan -> [(World, Plan)]
build_plan' world plan acc []              = acc
build_plan' world plan acc (action:actions)
  | is_allowed == True = build_plan' world plan ((new_world, (action:plan)):acc) actions
  | otherwise          = build_plan' world plan acc actions
  where
    (new_world, is_allowed) = execute world action

possible_action :: Plan
possible_action =  [(y,x)| x <- [1..10], y <- ["pick", "drop"]]

execute :: World -> (String, Int) -> (World, Bool)
execute world ("pick", column) = pick world column
execute world ("drop", column) = dropp world column

pick :: World -> Int -> (World, Bool)
pick (World (world, grabber)) column
  | null curr_column = (error "Column is empty")
  | grabber == Clear = (World (new_world, Grabber new_grabber), True)
  | otherwise        = (World (world, grabber), False) -- consider throwing excep
  where
    new_world   = (world !!= (column, new_column)) -- make first elem of column empty
    new_grabber = head curr_column -- get the block to pick 
    new_column  = tail curr_column -- remove block from column
    curr_column = get_stack world column

dropp :: World -> Int -> (World, Bool)
dropp (World (world, block)) column
  | block == Clear = (World (world, block), False) -- Nothing to drop
  | otherwise      = (World (new_world, Clear) ,True)                     
   where
     new_world      = world !!= (column, new_column)
     new_column     = curr_column ++ [(grabber_to_block block)]
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

picked :: World
picked = world
  where
    (world, _) = pick initial_world 1

is_valid_world :: World -> Bool
is_valid_world world = and rules
 where
   (World (blocks, grabber)) = world
   rules = [world_size world,shape_not_top world Pyramid,
            shape_not_top world Ball]
-- Rules

-- Size of a world can't be more than 10
world_size :: World -> Bool
world_size (World (blocks, _))
 | length blocks > 10 = False
 | otherwise          = True

--Pyramids and balls cannot support anything (gen function)
shape_not_top :: World -> Shape -> Bool
shape_not_top world shape = not $ or check_shape_pos
 where
   (World (blocks, _)) = world
   check_shape_pos   = map pos_shape ignore_first_blocks 
   pos_shape column  = any (\x -> is_shape x shape) column
   ignore_first_blocks = map tail (filter (not . null) blocks) -- disrecard 1st pos

--Size = Large | Medium | Small | Tall | Wide


--  Very naive compare method
bigger_eq :: Size -> Size -> Bool
bigger_eq s1 s2
 | s1 == s2 = True
bigger_eq Large  _ = True
bigger_eq Medium _ = True
bigger_eq Small  _ = True
bigger_eq Tall   _ = True
bigger_eq Wide   _ = True


-- HELPER -- Check if a block is of a specific shape
is_shape :: Block -> Shape -> Bool
is_shape (Block x _ _) shape
  | x == shape = True
  | otherwise  = False


