module PlannerFact where

import DataStructure


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



data Fact = OnTop Block Block Bool | LeftOf String String Bool |
            RightOf String String Bool | InGrabber String Bool |
            IsBlock String Bool | IsColumn String Bool


check_fact :: World -> Fact -> Bool
check_fact w (OnTop b1 b2 bool) = is_on_top w (OnTop b1 b2 bool)




top :: World -> Block -> Bool
top w b = b `elem` [head list | list <-blocks]
  where
    (World (blocks, _)) = w

is_on_top :: World -> Fact -> Bool
is_on_top w (OnTop b1 b2 bool)
 | col_b1 == col_b2 = on_top
 | otherwise        = False
  where
    (World (blocks, _)) = w
    col_b1 = filter (b1 `elem`) blocks
    col_b2 = filter (b2 `elem`) blocks
    on_top = index_on_top (elemIndex b1 col_b1) (elemIndex b2 col_b1)
is_on_top w (OnTop b1 (Floor i)) = b1 ==last b1 (blocks !! i)
  where
     (World (blocks, _)) = w
is_on_top w (OnTop (Floor _) _ ) = False
    


index_on_top (Just x) (Just y) = x == (y-1)
index_on_top Nothing Nothing   = False
index_on_top Nothing _         = False
index_on_top _       Nothing   = False
