module DataStructureFacts where

-- This module shows how we represent the world of blocks on a floor

data Shape = Square | Rectangle | Pyramid |
             Ball | Box deriving (Show, Eq)
                    
data Color = Red | Black | Blue |
             Green | Yellow | White deriving (Show, Eq)

data Size = Large | Medium |
            Small | Tall | Wide deriving (Show, Eq)
                   
data Block = Block Name Shape Size Color | Floor Int deriving (Show, Eq)

data Name = String deriving (Show, Eq)

data Grabber = Clear | Grabber Block deriving (Show, Eq)
                    
newtype World = World ([[Block]], Grabber) deriving (Show, Eq)

type Plan = [(String, Int)]
