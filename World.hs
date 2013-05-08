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

data Grabber = Nothing | Grabber Block
             deriving (Show, Eq)

data Stack =  Empty | Stack [Block]
           deriving (Show, Eq)
                    
data World =  World [Stack] Grabber
           deriving (Show, Eq)

type Plan = [(String, Int)] -- consider changing to action
        --  deriving(Show)

type GrabberState = (Bool, Grabber)

--blankWorld :: World
--blankWorld = World $ replicate 10 (Empty Nothing)



planner :: World -> World -> [World] -> [[(World, Plan)]] -> [(World, Plan)] -> Plan
--lan worldInit worldEnd = plan [] [[(World, Plan)]] []
planner world1 world2 acc [] [] = (error "Planner called with empty lists")
planner world1 world2 acc (((w, plan):xs):xss) stack1
  | world1 == world2 = reverse plan
  | w `elem` acc     = planner world1 world2 acc (xs:xss) stack1 
  | otherwise        = planner w world2 (w:acc) (xs:xss) ((build_plan w plan)++stack1)
planner world1 world2 acc ([]:xss) stack1 = planner world1 world2 acc xss stack1
planner world1 world2 acc [[]] stack1 = planner world1 world2 acc [[]] (reverse stack1) 

build_plan :: World -> Plan -> [(World, Plan)]
build_plan  world plan = build_plan' world plan [] possible_action

build_plan' :: World -> Plan -> [(World, Plan)] -> Plan -> [(World, Plan)]
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

getState :: State GrabberState Grabber
getState = do
  (_, score) <- get
  return score

--setState :: Bool -> Grabber -> State GrabberState Grabber
--setState bool state = do
 -- put (bool, state)


dropp :: World -> Int ->(World, Bool)
dropp world column = (world, True) -- dummy


pick :: World -> Int ->(World, Bool)
pick world column = (world, True) -- dummy
