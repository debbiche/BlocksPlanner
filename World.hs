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

data Stack = Stack [Block]
           deriving (Show, Eq)
                    
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
pick (world, Nothing) column = ((new_world, Just new_grabber), True) -- dummy
  where
    new_world   = (world !! column) !!= (0, Nothing) -- make first elem of column empty
    new_grabber = (world !! column) !! 0 
pick (world, grabber) column = ((world, grabber), False)


dropp :: ([Stack], Maybe Block) -> Int -> (([Stack], Maybe Block), Bool)
dropp (world, grabber) column = ((world, grabber), True) -- dummy


-- Updates the ith element in a list with v    
(!!=) :: [a] -> (Int ,a) -> [a]
(x:xs) !!= (0,el) = (el:xs)
(x:xs) !!= (n,el) = (x:( xs !!= (n-1,el))) 

