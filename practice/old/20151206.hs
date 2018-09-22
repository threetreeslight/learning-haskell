-- Haskell

-- note
-- whereのありがたみをちゃんと理解できていない
-- Pattern matchの基礎であるMaybe型をしっかりやり直す
-- folrの考え方をやり直す
-- 型シノニム
-- uncurry忘れかけてない？

-- 次回復習はここからrepeat replicate cycle
-- 次回は直交座標と極座標 https://skami.iocikun.jp/computer/haskell/web_lecture/for_programmer/cartesian_polar.html

import Data.List
import Data.Either

-- ----------------------------------------
-- recursible type
-- ----------------------------------------

-- data Apple = Leaf | Flower | Fruit | Branch Apple Apple deriving Show
-- type Basket = [Apple]
-- 
-- price1 :: Apple -> Int
-- price1 Leaf = 50
-- price1 Flower = 80
-- price1 Fruit = 100
-- price1 (Branch a1 a2) = 20 + price1 a1 + price1 a2
-- 
-- 
-- price::Basket -> Int
-- price = sum . map price1
-- 
-- dfs :: Apple -> [Apple]
-- dfs (Branch a1 a2) = dfs a1 ++ dfs a2
-- dfs a = [a]
-- 
-- bfs :: Apple -> [Apple]
-- bfs = concat . bfsl . (: [])
-- 
-- bfsl :: [Apple] -> [[Apple]]
-- bfsl = unfoldr $ \as -> if null as then Nothing else let
--         (ats, brs) = partitionEithers $ map branch as in
--         Just (ats, concat brs)
-- 
-- branch :: Apple -> Either Apple [Apple]
-- branch (Branch a1 a2) = Right [a1, a2]
-- branch a = Left a

data Matryoshika = Term | Nest Matryoshika deriving Show

matDepth :: Matryoshika -> Integer
matDepth Term = 1
matDepth (Nest mt) = 1 + matDepth mt

-- ----------------------------------------
-- 再帰的な多相型
-- ----------------------------------------

data Apple = Leaf | Flower | Fruit deriving Show
data Tree a = Branch (Tree a) (Tree a) | Single (Tree a) | Atom a deriving Show

priceA :: Apple -> Int
priceA Leaf = 50
priceA Flower = 80
priceA Fruit = 100

price1 :: Tree Apple -> Int
price1 (Atom a) = priceA a
price1 (Branch t1 t2) = 20 + price1 t1 + price1 t2

dfs :: Tree a -> [a]
dfs (Atom a) = [a]
dfs (Single t1) = dfs t1
dfs (Branch t1 t2) = dfs t1 ++ dfs t2

-- dfs $ Branch (Branch (Atom Fruit) (Atom Leaf)) (Single (Atom Flower))


-- ----------------------------------------
-- List
-- ----------------------------------------

data List a = Nil | a :~ List a deriving Show

mapL :: (a -> b) -> List a -> List b
mapL f (x :~ xs) = f x :~ mapL f xs
mapL _ _ = Nil

data ListWithDefault a b = Default b | a :~~ ListWithDefault a b deriving Show

fetchDefault :: ListWithDefault a b -> b
fetchDefault (a :~~ b) = fetchDefault b
fetchDefault (Default b) = b






