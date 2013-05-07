module World where

import Data.List
import Data.Maybe

data Shape = Square | Rectangle | Pyramid | Ball | Box
           deriving (Show)
          
data Color = Red | Black | Blue | Green | Yellow | White
           deriving (Show)
                    
data Form = Form { shape :: Shape, color :: Color }
            deriving (Show)

data Object = Maybe Form
            deriving (Show)

--data Block = [Object]

data World =  World [[Maybe Object]]
           deriving (Show)
                    
blankWorld :: World
blankWorld = World $ replicate 9 (replicate 10 Nothing)
