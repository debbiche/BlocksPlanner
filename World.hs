module World where

import Data.List
import Data.Maybe

data Shape = Square | Rectangle | Pyramid | Ball | Box
           deriving (Show)
          
data Color = Red | Black | Blue | Green | Yellow | White
           deriving (Show)

data Size = Large | Medium | Small | Tall | Wide
          deriving (Show)
                    
data Block = Block Shape Size Color 
            deriving (Show)

data Grabber = Nothing | Grabber Block
             deriving (Show)

data Stack =  Empty | Stack [Block]
           deriving (Show)
data World =  World [Stack] Grabber
           deriving (Show)
                    
--blankWorld :: World
--blankWorld = World $ replicate 10 (Empty Nothing)
