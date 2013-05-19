module DataStructureFacts where

import Data.List

-- This module shows how we represent the world of blocks on a floor

data Shape = Square | Rectangle | Pyramid |
             Ball | Box deriving (Show, Eq)
                    
data Color = Red | Black | Blue |
             Green | Yellow | White deriving (Show, Eq)

data Size = Large | Medium |
            Small | Tall | Wide deriving (Show, Eq)
                   
data Block = Block Name Shape Size Color | Floor Int deriving (Show, Eq)

type Name = String

data Grabber = Clear | Grabber Block deriving (Show, Eq)
                    
data World = World ([[Block]], Grabber) deriving (Show, Eq)

type Plan = [(String, Int)]


column_index_of :: Block -> World -> Maybe Int
column_index_of block (World (blocks, _)) = findIndex (block `elem`) blocks