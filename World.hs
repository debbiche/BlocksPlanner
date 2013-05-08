module World where

import Data.List
import Data.Maybe

data Shape = Square | Rectangle | Pyramid | Ball | Box
           deriving (Show)
          
data Color = Red | Black | Blue | Green | Yellow | White
           deriving (Show)
                    
data Form = Form Shape Color 
            deriving (Show)

data Stack = Empty | Stack [Form]
           deriving (Show)
             
data World =  World [Stack]
           deriving (Show)
                    
blankWorld :: World
blankWorld = World $ replicate 10 Empty
