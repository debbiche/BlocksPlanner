module World where

import Data.List
import Data.Maybe
import Data.String
import Control.Monad.State

data Shape = Square | Rectangle | Pyramid | Ball | Box
           deriving (Show, Eq)
                    
data Color = Red | Black | Blue | Green | Yellow | White
           deriving (Show, Eq)

data Size = Large | Medium | Small | Tall | Wide
          deriving (Show, Eq)
                   
data Block = Block Shape Size Color 
           deriving (Show, Eq)

type Grabber = Maybe Block
    --        deriving (Show, Eq)

type Stack = [Maybe Block]
       --    deriving (Show, Eq)
                    
data World = World ([Stack], Grabber)
           deriving (Show, Eq)

type Plan = [(String, Int)] -- consider changing to action
        --  deriving(Show)

-- A world = ([Stack], Grabber)



planner :: ([Stack], Maybe Block) -> ([Stack], Maybe Block) -> [([Stack], Maybe Block)]
           -> [[(([Stack], Maybe Block), Plan)]] -> [(([Stack], Maybe Block), Plan)] -> Plan
--lan worldInit worldEnd = plan [] [[(World, Plan)]] []
planner world1 world2 acc [] [] = (error "Planner called with empty lists")
planner world1 world2 acc (((w, plan):xs):xss) stack1
  | world1 == world2 = reverse plan
  | w `elem` acc     = planner world1 world2 acc (xs:xss) stack1 
  | otherwise        = planner w world2 (w:acc) (xs:xss) ((build_plan w plan)++stack1)
planner world1 world2 acc ([]:xss) stack1 = planner world1 world2 acc xss stack1
planner world1 world2 acc [[]] stack1 = planner world1 world2 acc [[]] (reverse stack1) 

build_plan :: ([Stack], Maybe Block) -> Plan -> [(([Stack], Maybe Block), Plan)]
build_plan  world plan = build_plan' world plan [] possible_action

build_plan' :: ([Stack], Maybe Block) -> Plan -> [(([Stack], Maybe Block), Plan)]
               -> Plan -> [(([Stack], Maybe Block), Plan)]
build_plan' world plan acc []              = acc
build_plan' world plan acc (action:actions)
  | is_allowed == True = build_plan' world plan ((new_world, (action:plan)):acc) actions
  | otherwise          = build_plan' world plan acc actions
  where
    (new_world, is_allowed) = execute world action

possible_action :: Plan
possible_action =  [(y,x)| x <- [1..10], y <- ["pick", "drop"]]

execute :: ([Stack], Maybe Block) -> (String, Int) -> (([Stack], Maybe Block), Bool)
execute world ("pick", column) = pick world column
execute world ("drop", column) = dropp world column

pick ::([Stack], Maybe Block) -> Int -> (([Stack], Maybe Block), Bool)
pick (world, Nothing) column = ((new_world, new_grabber), True) -- dummy
  where
    new_world   = world !!= (column, new_column) -- make first elem of column empty
    new_grabber = (world !! column) !! 0
    new_column  = (get_stack world column) !!= (0, Nothing)
pick (world, grabber) column = ((world, grabber), False)


dropp :: ([Stack], Maybe Block) -> Int -> (([Stack], Maybe Block), Bool)
dropp (world, Nothing) column = ((world, Nothing), False) -- Nothing to drop
dropp (world, Just block) column = ((new_world, Just block) ,True)
  where
    new_world      = world !!= (column, new_column)
    new_column     = current_column !!= (index, Just block)
    Just index     = elemIndex Nothing current_column
    current_column = get_stack world column -- column to drop on
    


-- Updates the ith element in a list with v    
(!!=) :: [a] -> (Int ,a) -> [a]
(x:xs) !!= (0,el) = (el:xs)
(x:xs) !!= (n,el) = (x:( xs !!= (n-1,el))) 


get_stack :: [Stack] -> Int -> Stack
get_stack stack col = get_stack' stack col 0
  where
    get_stack' [] _ acc = (error "Invalid Stack Column")
    get_stack' (x:xs) col acc
      | col == acc = x
      | otherwise  = get_stack' xs col acc
