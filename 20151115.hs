-- Haskell

-- note
-- whereのありがたみをちゃんと理解できていない
-- Pattern matchの基礎であるMaybe型をしっかりやり直す
-- folrの考え方をやり直す
-- 型シノニム
-- uncurry忘れかけてない？

-- 次回復習はここからrepeat replicate cycle
-- 次回は直交座標と極座標 https://skami.iocikun.jp/computer/haskell/web_lecture/for_programmer/cartesian_polar.html

import Data.Char




-- type Cartesian = (Double, Double)
-- data Cartesian = Cartesian Double Double deriving Show
-- 
-- mulC :: Cartesian -> Double -> Cartesian
-- mulC (Cartesian x y) n = Cartesian (x * n) (y * n)
-- 
-- data Polar = Polar Double Double deriving Show
-- 
-- mulP :: Polar -> Double -> Polar
-- mulP (Polar d r) n = Polar (d * n) r

-- ----------------------------------------
-- Point is Point
-- ----------------------------------------

-- data Point = Cartesian Double Double | Polar Double Double deriving Show
-- 
-- mul :: Point -> Double -> Point
-- mul (Cartesian x y) n = Cartesian (x * n) (y * n)
-- mul (Polar d r) n = Polar (d * n) r


data Point = Point Double Double deriving Show
data Shape = Circle Point Double | Square Point Double | Rectangle Point Double Double deriving Show

square :: Shape -> Shape
square (Rectangle (Point x y) a b) | a == b = Square (Point x y) a
square s = s

-- ----------------------------------------
-- 関数としての値構築子
-- ----------------------------------------

data Cell = Cell Char Int deriving Show

column :: Cell -> Char
column (Cell c _) = c

row :: Cell -> Int
row (Cell _ r) = r


toJustUpper :: Char -> Maybe Char
-- toJustUpper c = Just (toUpper c)
-- toJustUpper c = (Just . toUpper) c
toJustUpper = Just . toUpper

-- ----------------------------------------
-- 値構築演算子
-- ----------------------------------------

data Cap = Red | Blue | Yellow deriving Show
-- data Order = Change Cap Cap deriving Show

-- recreation :: Cap -> [Order] -> Cap
-- recreation c [] = c
-- recreation Red (Change Red c : os) = recreation c os
-- recreation Blue (Change Blue c : os) = recreation c os
-- recreation Yellow (Change Yellow c : os) = recreation c os
-- recreation c (_ : os) = recreation c os

data Order = Cap :-> Cap deriving Show
recreation :: Cap -> [Order] -> Cap
recreation c [] = c
recreation Red (Red :-> c : os) = recreation c os
recreation Blue (Blue :-> c : os) = recreation c os
recreation Yellow (Yellow :-> c : os) = recreation c os
recreation c (_ : os) = recreation c os

-- data CharCount = CharCount Char Integer deriving Show
data CharCount = Char :*: Int deriving Show
parseToCharCount :: CharCount -> String
parseToCharCount (c :*: i) = replicate i c

-- ----------------------------------------
-- 多層型
-- ----------------------------------------

data Twice a = Twice a a deriving Show

mapTwice :: (a -> b)  -> Twice a -> Twice b
mapTwice f (Twice x y) = Twice (f x) (f y)

data Rep a = Rep Int a deriving Show
toList :: Rep a -> [a]
toList (Rep n x) = replicate n x

data Option a b = Single a | Option a b deriving Show

human :: Option String Int -> String
human (Single n) = n
human (Option n a) = n ++ "(" ++ show a ++ ")"

data Three a = Three a a a deriving Show

mapThree :: (a -> b) -> (Three a) -> (Three b)
mapThree f (Three a b c) = Three (f a) (f b) (f c)

-- ----------------------------------------
-- タプル
-- ----------------------------------------

-- data (,) a b = (,) a b deriving Show
data Tuple a b = Tuple a b deriving Show
fstT :: Tuple a b -> a
fstT (Tuple x _) = x

sndT :: Tuple a b -> b
sndT (Tuple _ y) = y

-- ----------------------------------------
-- Maybe
-- ----------------------------------------

data Maybe' a = Just' a | Nothing' deriving Show
fromMaybe' :: a -> Maybe' a -> a
fromMaybe' _ (Just' x) = x
fromMaybe' d _ = d

maybe' :: b -> (a -> b) -> Maybe' a -> b
maybe' _ f (Just' x) = f x
maybe' d _ _ = d

-- ----------------------------------------
-- Eihter
-- ----------------------------------------

type Id = Either Int String
name :: Id -> [(Id, String)] -> Maybe String
-- name i t = lookup i t
name = lookup

users :: [(Id, String)]
users = [
        (Right "yoshio", "Yoshio Yamada"),
        (Right "yoshio2", "Yoshio Yamada"),
        (Left 4492, "Tatsuya Yamashiro"),
        (Right "keiko", "Keiko Koike"),
        (Left 8855, "Satoru Hananakajima")
        ]

data Either' a b = Left' a | Right' b deriving Show

either' :: (a -> c) -> (b -> c) -> Either' a b -> c
either' f _ (Left' x) = f x
either' _ g (Right' y) = g y






