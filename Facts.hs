module Facts where

import DataStructureFacts
import Data.List


a = Block "a" Rectangle Tall Blue
b = Block "b" Ball Large White
c = Block "c" Square Large Red
d = Block "d" Pyramid Large Green
e = Block "e" Box Large White
f = Block "f" Rectangle Wide Black
g = Block "g" Rectangle Wide Blue
h = Block "h" Rectangle Wide Red
i = Block "i" Pyramid Small Yellow
j = Block "j" Box Large Red
k = Block "k" Ball Small Yellow
l = Block "l" Box Medium Red
m = Block "m" Ball Medium Blue

s1 = [b,a]
s2 = [d,c]
s3 = [i,h,g,f,e]
s4 = [k,j]
s5 = [m,l]
s55 = [l]
empty = []

testLeftOf = LeftOf a c True
testOnTop = OnTop b a True
testAbove = Above i g True
notAbove = Above g i True
under = Under e g False
andd = And [Fluent testOnTop, Fluent testAbove]
orr = Or [Fluent notAbove]
testFluent = Fluent (OnTop b m True)
--testFluent = Fluent (OnTop b d True)

testWorld = World ([empty,s1,s2,empty,s3,empty,empty,s4,empty,s5], Grabber i)
--testWorld = World ([s1,s2,empty,empty,empty,empty,empty,empty,empty,empty],Clear)


data Fact = OnTop Block Block Bool | LeftOf Block Block Bool |
            RightOf Block Block Bool | InGrabber Block Bool |
            Above Block Block Bool | Under Block Block Bool
                                     deriving (Show, Eq)
          --  IsBlock String Bool | IsColumn String Bool

data Fluent = Fluent Fact | And [Fluent] | Or [Fluent]
            deriving (Show, Eq)

satisfies :: World -> Fluent -> Bool
satisfies world fluent = check_fluent world fluent

check_fluent :: World -> Fluent -> Bool
check_fluent w (Fluent f)     = check_fact w f
check_fluent w (And fluents)  = and $ map (check_fluent w) fluents
check_fluent w (Or fluents)   = or $ map (check_fluent w) fluents

check_fact :: World -> Fact -> Bool
check_fact w (OnTop b1 b2 bool)   = is_on_top w (OnTop b1 b2 bool) == bool
check_fact w (LeftOf b1 b2 bool)  = is_left_of w (LeftOf b1 b2 bool) == bool
check_fact w (RightOf b1 b2 bool) = is_right_of w (RightOf b1 b2 bool) == bool
check_fact w (InGrabber b1 bool)  = is_in_grabber w (InGrabber b1 bool) == bool
check_fact w (Above b1 b2 bool)   = is_above w (Above b1 b2 bool) == bool
check_fact w (Under b1 b2 bool)   = not $ is_above w (Above b1 b2 bool) == bool

top :: World -> Block -> Bool
top w b = b `elem` [head list | list <-blocks]
  where
    (World (blocks, _)) = w

is_on_top :: World -> Fact -> Bool
is_on_top w (OnTop (Floor _) _ _) = False
is_on_top w (OnTop b1 (Floor i) bool) = b1 == last (blocks !! i)
  where
     (World (blocks, _)) = w
is_on_top w (OnTop b1 b2 bool)
 | null f1 || null f2 = False
 | col_b1 == col_b2 = on_top
 | otherwise        = False
  where
    (World (blocks, _)) = w
    f1 = filter (b1 `elem`) blocks
    f2 = filter (b2 `elem`) blocks
    col_b1 = head f1
    col_b2 = head f2
    on_top = index_on_top (elemIndex b1 col_b1) (elemIndex b2 col_b1)

compare_maybe :: Maybe Int -> Maybe Int -> Int -> Bool
compare_maybe Nothing _ _ = False
compare_maybe _ Nothing _ = False
compare_maybe (Just x) (Just y) r = x - y == r

is_left_of :: World -> Fact -> Bool
is_left_of w (LeftOf b1 b2 bool) = 
  compare_maybe (column_index_of b2 w) (column_index_of b1 w) 1

is_right_of :: World -> Fact -> Bool
is_right_of w (RightOf b1 b2 bool) = 
  compare_maybe (column_index_of b2 w) (column_index_of b1 w) (-1)

is_in_grabber :: World -> Fact -> Bool
is_in_grabber (World (blocks, Clear)) _ = False
is_in_grabber (World (blocks, Grabber block)) (InGrabber b bool) = (block == b) == bool

is_above :: World -> Fact -> Bool
is_above w (Above (Floor _) _ _) = False
is_above w (Above b1 (Floor i) bool) = b1 == last (blocks !! i)
  where
     (World (blocks, _)) = w
is_above w (Above b1 b2 bool)
 | col_b1 == col_b2 = above
 | otherwise        = False
  where
    (World (blocks, _)) = w
    col_b1 = head $ filter (b1 `elem`) blocks
    col_b2 = head $ filter (b2 `elem`) blocks
    above = index_above (elemIndex b1 col_b1) (elemIndex b2 col_b1)

index_above :: Maybe Int -> Maybe Int -> Bool
index_above (Just x) (Just y) = x < y
index_above Nothing Nothing   = False
index_above Nothing _         = False
index_above _       Nothing   = False

index_on_top :: Maybe Int -> Maybe Int -> Bool
index_on_top (Just x) (Just y) = x == (y-1)
index_on_top Nothing Nothing   = False
index_on_top Nothing _         = False
index_on_top _       Nothing   = False
