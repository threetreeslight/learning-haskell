-- Haskell Revision

-- note
-- whereのありがたみをちゃんと理解できていない
-- Pattern matchの基礎であるMaybe型をしっかりやり直す
-- folrの考え方をやり直す
-- 型シノニム
-- uncurry忘れかけてない？

-- 次回復習はここからrepeat replicate cycle
-- new workはパーサ: 計算機
-- https://skami.iocikun.jp/computer/haskell/web_lecture/for_programmer/parser_calculator.html

import Prelude hiding((*>))
import Data.Maybe
import Data.Char

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

-- ----------------------------------------
-- concatMap
-- ----------------------------------------

concatMap' :: (a -> [b]) -> [a] -> [b]
-- concatMap' f = concat . map f
-- concatMap' f = (concat .) $ map f
concatMap' = (concat .) . map

concatMapF, concatMapRaw :: (a -> [b]) -> [a] -> [b]

-- concatMapF f = foldr (\x xs -> f x ++ xs) []
-- concatMapF f = foldr (\x xs -> (++) (f x) xs) []
-- concatMapF f = foldr (\x -> (++) (f x)) []
-- concatMapF f = foldr (\x -> (++) $ f x) []
concatMapF f = foldr ((++) . f) []

concatMapRaw f (x : xs) =  f x ++ concatMapRaw f xs
concatMapRaw _ _ = []

