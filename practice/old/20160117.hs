-- Haskell

-- Start with 構文: レコード構文
-- https://skami.iocikun.jp/computer/haskell/web_lecture/for_programmer/record_syntax.html
-- End on 構文: 代数的データ型のエクスポート


import Data.Tree (Tree(..))

-- ----------------------------------------
-- record style
-- ----------------------------------------

-- data Circle = Circle Double Double Double deriving Show
data Circle = Circle { centerX :: Double, centerY :: Double, radius :: Double } deriving Show

area :: Circle -> Double
-- area (Circle _ _ r) = r ^ 2 * pi
area Circle { radius = r } = r ^ 2 * pi

inside :: Circle -> (Double, Double) -> Bool
-- inside (Circle cx cy r) (x, y) =  (x - cx) ^ 2 + (y - cy) ^ 2 <= r ^ 2
inside Circle { centerX = cx, centerY = cy, radius = r} (x,y) = (x - cx) ^ 2 + (y - cy) ^ 2 <= r ^ 2

moveH :: Circle -> Double -> Circle
-- moveH (Circle cx cy r) dx = Circle (cx + dx) cy r
moveH c@Circle { centerX = cx } dx = c { centerX = cx + dx }

data Rectangle
        = RectBR { topL :: (Double, Double), botR :: (Double, Double) }
        | RectWH { topL :: (Double, Double), with:: Double, hight :: Double }

data Human = Human { name :: String, age :: Integer } deriving Show
-- data Human = Human String Integer deriving Show

describeHuman :: Human -> String
describeHuman Human { name = n, age = a} = n ++ "(" ++ show(a) ++ ")"

-- ----------------------------------------
-- create Module
-- ----------------------------------------

cons :: Integer -> Integer -> Integer
cons x y = 2 ^ x * 3 ^ y

unpower :: Integer -> Integer -> Integer
unpower n x
        | x `mod` n /= 0 = 0
        | otherwise = 1 + unpower n (x `div` n)

uncons :: Integer -> (Integer, Integer)
uncons xy = (unpower 2 xy, unpower 3 xy)







