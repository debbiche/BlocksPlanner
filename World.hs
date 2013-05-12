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
dropp (World (world, Clear)) column = (World (world, Clear), False) -- Nothing to drop
dropp (World (world, block)) column = (World (new_world, block) ,True)
   where
     new_world      = world !!= (column, new_column)
     new_column     = current_column ++ [(grabber_to_block block)]
     current_column = get_stack world column -- column to drop on

grabber_to_block :: Grabber -> Block
grabber_to_block (Grabber (Block x y z)) = (Block x y z)


-- Updates the ith element in a list with v    
(!!=) :: [a] -> (Int ,a) -> [a]
(x:xs) !!= (0,el) = (el:xs)
(x:xs) !!= (n,el) = (x:( xs !!= (n-1,el)))
 

get_stack :: [[Block]] -> Int -> [Block]
get_stack blocks col = blocks !! col 


get_blocks :: World -> [[Block]]
get_blocks (World (blocks, _)) = blocks
