-- Haskell Revision

-- whereのありがたみをちゃんと理解できていない
-- Pattern matchの基礎であるMaybe型をしっかりやり直す
-- folrの考え方をやり直す

-- 次回復習はここからrepeat replicate cycle
-- concatMapの演習問題が解けませんでした

import Data.Maybe
import Data.Char

-- ----------------------------------------
-- Parser : basic function
-- ----------------------------------------

type Parse a = String -> [(a, String)]

succeed :: a -> Parse a
succeed v i = [(v, i)]

-- read 1 char
check :: (Char -> Bool) -> Parse Char
check p (c : cs) | p c = [(c, cs)]
check _ _ = []

-- read target char
char :: Char -> Parse Char
char = check . (==)

-- return two parse result
alt :: Parse a -> Parse a -> Parse a
(p1 `alt` p2) i = p1 i ++ p2 i

-- Modify parser return
build :: Parse a -> (a -> b) -> Parse b
-- build p f i = [(f x, r) | (x, r) <- p i]
build [(x, r)] f = [(f x, r)]






-- ----------------------------------------
-- List comprehension
-- ----------------------------------------

tcomb = [ [x,y,z] |
        x <- take 5 [1..],
        y <- take 5 [1..],
        z <- take 5 [1..],
        isTriangle x y z,
        isOrderByAsc x y z
        ]

isTriangle :: Integer -> Integer -> Integer -> Bool
isTriangle x y z
        | x^2 + y^2 == z^2 = True
        | y^2 + z^2 == x^2 = True
        | z^2 + x^2 == y^2 = True
        | otherwise = False

isOrderByAsc :: Integer -> Integer -> Integer -> Bool
isOrderByAsc x y z = if x < y && y < z then True else False

-- ----------------------------------------
-- filter made by concatMap
-- ----------------------------------------

filterC :: (a -> Bool) -> [a] -> [a]
filterC p = concatMap $ \x -> if p x then [x] else []

-- ----------------------------------------
-- concatMap
-- ----------------------------------------

concatMap' :: (a -> [b]) -> [a] -> [b]
-- concatMap' f = concat . map f
-- concatMap' f = (concat .) $ map f
concatMap' = (concat .) . map

-- 解けなかった
-- concatMapRaw (replicate 3) "hello"
-- concatMapRaw :: (a -> [b]) -> [a] -> [b]
-- concatMapRaw f (x : xs) = ((concat .) . f x) : concatMapRaw f xs
-- concatMapRaw _ _ = []

-- concatMapF


-- ----------------------------------------
-- repeat replicate cycle
-- ----------------------------------------

repeatRaw, repeatI :: a -> [a]
repeatRaw x = x : repeatRaw x

repeatI = iterate id


replicateRaw, replicateT :: Int -> a -> [a]
replicateRaw n x | n > 0 = x : replicateRaw (n -1) x
replicateRaw _ _ = []

replicateT n = take n . repeat

cycleRaw, cycleC :: [a] -> [a]
cycleRaw xs = xs ++ cycleRaw xs
cycleC = concat . repeat




