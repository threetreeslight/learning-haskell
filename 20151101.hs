-- Haskell Revision

-- note
-- whereのありがたみをちゃんと理解できていない
-- Pattern matchの基礎であるMaybe型をしっかりやり直す
-- folrの考え方をやり直す
-- 型シノニム
-- uncurry忘れかけてない？

-- 次回復習はここからrepeat replicate cycle
-- 次回は直交座標と極座標 https://skami.iocikun.jp/computer/haskell/web_lecture/for_programmer/cartesian_polar.html

import Prelude hiding((*>))
import Data.Maybe
import Data.Char

-- ----------------------------------------
-- Human and product -> Abstracted data type
-- ----------------------------------------

-- name , age
-- type Human = (String, Int)

-- age :: Human -> String
-- age (n, a) = n ++ " is " ++ show a ++ " years old."

-- masuo :: Human
-- masuo = ("Masuo", 32)

-- type Product = (String, Int)
-- price :: Product -> String
-- price (n, p) = n ++ " is " ++ show p ++ " yen."

-- iphone6s :: Product
-- iphone6s = ("iPhone6s", 99000)

data Human = Human String Int deriving Show

age :: Human -> String
age (Human n a) = n ++ " is " ++ show a ++ " years old."

masuo :: Human
masuo = Human "Masuo" 32

data Product = Product String Int deriving Show
price :: Product -> String
price (Product n p) = n ++ " is " ++ show p ++ "yen."

iphone6s :: Product
iphone6s = Product "iPhone 6s" 99000


data Building = Building String Integer deriving Show

extractBuildingHight :: Building -> String
extractBuildingHight (Building n h) = n ++ " is " ++ show h


-- ----------------------------------------
-- enum
-- ----------------------------------------

type RGB = (Int, Int, Int)
data Suit = Spade | Heart | Diamond | Club deriving Show

data Color = Black | Red

rgb :: Suit -> RGB
rgb = toRGB . color

color :: Suit -> Color
color Spade   = Black
color Heart   = Red
color Diamond = Red
color Club    = Black

toRGB :: Color -> RGB
toRGB Black = (0,0,0)
toRGB Red = (0xff,0,0)

data Sex = Male | Female
compellation :: (Sex, String) -> String
compellation (Male, name) = "Mr. " ++ name
compellation (_, name) = "Mrs. " ++ name

-- data Bool = True | False

data Date = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving Show
dayType :: Date -> String
dayType Sunday = "play"
dayType _ = "work"







-- ----------------------------------------
-- parser : calculator
-- ----------------------------------------

type Op = Integer -> Integer -> Integer

op, ad, sb, ml, dv :: Parse Op
op = ad `alt` sb `alt` ml `alt` dv
ad = char '+' `build` const (+)
sb = char '-' `build` const (-)
ml = char '*' `build` const (*)
dv = char '/' `build` const (div)

expr :: Parse Integer
expr = (term >*> op >*> term) `build` \((x, o), y) -> x `o` y

term :: Parse Integer
term = number `alt` (char '(' *> expr >* char ')')

calc :: String -> Maybe Integer
calc = parse expr


-- ----------------------------------------
-- Parser : Number parser
-- ----------------------------------------

number :: Parse Integer
number = list1 (check isDigit) `build` read

parse :: Parse a -> String -> Maybe a
-- parse p = listToMaybe . map fst . (p >* eof)
-- parse p = ((listToMaybe . map fst) . ) $ p >* eof
-- parse p = ((listToMaybe . map fst) . ) $ (>* eof) p
parse = ((listToMaybe . map fst) . ) .( >* eof)

-- Space区切りの文字列リスト
spaces1 :: Parse ()
-- spaces1 " 123 456 789"
-- [((),"123 456 789")]
spaces1 = list1 (check isSpace) `build` const ()

numbers :: Parse [Integer]
numbers = (number >*> list (spaces1 *> number)) `build` uncurry (:)

-- コンマ区切りの数値リスト
spaces :: Parse()
spaces = list (check isSpace) `build` const ()
comma :: Parse ()
comma = (spaces >*> char ',' >*> spaces) `build` const ()

cnumbers :: Parse [Integer]
cnumbers = (number >*> list (comma *> number)) `build` uncurry (:)



-- ----------------------------------------
-- Parser : Parse list
-- ----------------------------------------

-- uncurry (:) (1,[2])
-- > [1,2]
-- (check isDigit >*> succeed "a") "1223"
-- > [(('1',"a"),"223")]
-- (check isDigit >*> succeed "a") `build` uncurry (:) $ "1223"
-- > [("1a","223")]

list, list1 :: Parse a -> Parse [a]
list p = succeed [] `alt` list1 p
list1 p = (p >*> list p) `build` uncurry (:)
-- list1 p = (p >*> (succeed [] `alt` list1 p)) `build` uncurry (:)

-- ----------------------------------------
-- Parser : basic function
-- ----------------------------------------

type Parse a = String -> [(a, String)]

-- succeed 123 "hello"
succeed :: a -> Parse a
succeed v i = [(v, i)]

-- read 1 char
-- check isDigit "123"
check :: (Char -> Bool) -> Parse Char
check p (c : cs) | p c = [(c, cs)]
check _ _ = []

-- read target char
-- char 'a' "abc"
char :: Char -> Parse Char
char = check . (==)

-- return two parse result
-- (char 'a' `alt` check isDigit) "123"
-- alt (char 'a') (check isDigit) "123"
-- alt :: (String -> [(a, String)]) -> (String -> [(a, String)]) -> String -> [(a, String)]
alt :: Parse a -> Parse a -> Parse a
(p1 `alt` p2) i = p1 i ++ p2 i

-- Modify parser return
-- build :: Parse a -> (a -> b) -> String -> [(b, String)]
build :: Parse a -> (a -> b) -> Parse b
build p f i = [(f x, r) | (x, r) <- p i]

-- Parser chain
-- (>*>) :: (String -> [(a, String)]) -> (String -> [(b, String)]) -> String -> [((a,b), String)]
-- (char 'a' >*> check isDigit) "a123"
(>*>) :: Parse a -> Parse b -> Parse (a,b )
(p1 >*> p2) i = [((x,y), r') | (x, r) <- p1 i, (y, r') <- p2 r]

(>*) :: Parse a -> Parse b -> Parse a
-- (p1 >* p2) i = ((p1 >*> p2) `build` fst) i
p1 >* p2 = (p1 >*> p2) `build` fst

(*>) :: Parse a -> Parse b -> Parse b
p1 *> p2 = (p1 >*> p2) `build` snd

-- check the string EOF
-- eof "" -> 終端
-- eof "a" -> 終端でない
eof :: Parse ()
-- eof :: String -> [((), String)]
eof "" = [((), "")]
eof _ = []

