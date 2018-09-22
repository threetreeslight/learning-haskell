-- 20150907
import Text.Read
import Data.List


-- ----------------------------------------
-- 関数iterateの定義
-- ----------------------------------------

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

--- 課題

--- myList x = 
myListNext :: Integer -> Integer
myListNext x = case x `mod` 3 of
        0 -> x `div` 3
        1 -> x * 2 + 1
        2 -> x * 2 + 2

myList :: Integer -> [Integer]
-- myList x = iterate myListNext x
myList x = x : myList (myListNext x)



-- ----------------------------------------
-- コラッツ数列
-- ----------------------------------------

--- takeTo :: (a -> Bool) -> [a] -> [a]
--- takeTo _ [] = []
--- takeTo p (x : xs) | p x = [x]
---                   | otherwise = x : takeTo p xs
--- 
--- fun x lst = if p x then [x] else x : lst
--- takeTo p (x : xs) = x `fun` takeTo p xs
--- 
--- (takeTo p) [] = []
--- (takeTo p) (x : xs) = x `fun` (takeTo p) xs
-- takeTo p = foldr (\x -> if p x then const [x] else (x :)) []
takeTo p = foldr (\x -> (x :) . if p x then const [] else id) []

---

collatsInf :: Integer -> [Integer]
--- collatsInf n = n : collatsInf (collatsNext n)
--- collatsInf = iterate collatsNext
collatsInf = iterate $ \n -> if even n then n `div` 2 else n * 3 + 1

collats :: Integer -> [Integer]
collats = takeTo (== 1) . collatsInf

collatsNext :: Integer -> Integer
collatsNext n
        | even n = n `div` 2
        | otherwise = n * 3 + 1

-- ----------------------------------------
-- 整数の列挙
-- ----------------------------------------

myEnumFromTo :: Integer -> Integer -> [Integer]
myEnumFromTo m n | m > n = []
myEnumFromTo m n = m : myEnumFromTo (m + 1) n

-- ----------------------------------------
-- 逆ポーランド記法電卓とたたみこみ(左)
-- ----------------------------------------

eg = [(False, 3), (True, 8), (False, 7)]
rbolishIter :: Integer -> [(Bool, Integer)] -> Integer
rbolishIter s [] = s
rbolishIter s ((False, n) : ns) = rbolishIter (s + n) ns
rbolishIter s ((_, n) : ns)  = rbolishIter (s * n) ns

rbolish = rbolishIter 0

rbolish' :: [(Bool, Integer)] -> Integer
rbolish' = foldl' (\s (b, n) -> (if b then (*) else (+)) s n) 0

rpolish1 :: Maybe [Integer] -> String -> Maybe [Integer]
rpolish1 (Just ns) s = case lookup s operators of
        Just o -> case ns of
                y : x : ns' -> Just $ x `o` y : ns'
                _ -> Nothing
        _ -> maybe Nothing (Just . (: ns)) $ readMaybe s
rpolish1 _ _ = Nothing

rpolish' :: [String] -> Maybe [Integer]
rpolish' = foldl' rpolish1 $ Just []


-- ----------------------------------------
-- 逆ポーランド記法電卓
-- ----------------------------------------

rpolishIter :: Maybe [Integer] -> [String] -> Maybe [Integer]
rpolishIter mns [] = mns
rpolishIter (Just ns) (s : ss) = case lookup s operators of
        Just o -> case ns of
                y : x : ns' -> rpolishIter (Just $ x `o` y : ns') ss
                _ -> Nothing
        _ -> rpolishIter (maybe Nothing (Just . (: ns)) $ readMaybe s) ss
rpolishIter _ _ = Nothing

rpolish :: [String] -> Maybe [Integer]
rpolish = rpolishIter $ Just []

-- ----------------------------------------
-- ポーランド記法とたたみこみ(右)
-- ----------------------------------------
--
-- Answer
--
-- bolish, bolish' :: [(Bool, Integer)] -> Integer
-- bolish [] = 0
-- bolish ((False, n) : ns) = n + bolish ns
-- bolish ((_, n) : ns) = n * bolish ns
-- bolish' = foldr (\(b, n) -> (if b then (*) else (+)) n) 0

example' = [(False, 3), (True, 8), (False, 7)]

bolish' :: [(Bool, Integer)] -> Integer
bolish' = foldr opshow 0

bolish :: [(Bool, Integer)] -> Integer
bolish [] = 0
bolish (x : xs) = opshow x $ bolish xs

opshow :: (Bool, Integer) -> Integer -> Integer
opshow (x,y) = (if x then (*) else (+)) y

example = ["*", "+", "1", "5", "+", "2", "3"]

polish' :: [String] -> Maybe [Integer]
polish' = foldr polish1 (Just [])


polish1 :: String -> Maybe [Integer] -> Maybe [Integer]
polish1 s (Just ns) = case lookup s operators of
      Just o -> case ns of
              x : y : ns' -> Just $ x `o` y : ns'
              _ -> Nothing
      _ -> maybe Nothing (Just . (: ns)) $ readMaybe s
polish1 _ _ = Nothing

polish :: [String] -> Maybe [Integer]
polish [] = Just []
polish (s : ss) = case lookup s operators of
        Just o -> case polish ss of
                Just (x : y : ns) -> Just $ x `o` y : ns
                _ -> Nothing
        _ -> case readMaybe s of
                Just n -> maybe Nothing (Just . (n :)) $ polish ss
                _ -> Nothing


operators :: [(String, Integer -> Integer -> Integer)]
operators = [("+", (+)), ("-", (-)), ("*", (*)), ("/", div)]

-- ----------------------------------------
-- たたみこみ(右) && たたみこみ(左)
-- ----------------------------------------

myFoldl _ s [] = s
myFoldl op s (x : xs) = myFoldl op (s `op` x) xs

sumIter :: Integer -> [Integer] -> Integer
sumIter s [] = s
sumIter s (x : xs) = sumIter (s + x) xs

-- myFoldr _ v [] = v
-- myFoldr op v (x : xs) = x `op` myFolder op v xs

-- ----------------------------------------
-- リストの長さ
-- ----------------------------------------

len :: a -> Int -> Int
len = const (1 +)

myLength :: [a] -> Int
-- myLength [] = 0
-- myLength (_ : xs) = 1 + myLength xs
myLength xs = sum $ map (const 1) xs

-- ----------------------------------------
-- リストの要素の総積
-- ----------------------------------------
myMaximum :: [Integer] -> Integer
myMaximum [] = 0
myMaximum [x] = x
myMaximum (x : xs) = x `max` myMaximum xs

myProduct :: [Integer] -> Integer
myProduct [] = 1
myProduct (x : xs) = x * myProduct xs

-- ----------------------------------------
-- リストの要素の総和
-- ----------------------------------------
mySum :: [Integer] -> Integer
mySum [] = 0
mySum (x : xs) = x + mySum xs

-- ----------------------------------------
-- リストのパターンマッチ
-- ----------------------------------------

plural :: [a] -> Bool
-- より厳しい物を先に持ってくることで、ワイルドカードで処理することが容易になる
-- 構文糖を利用することによってより短く表現することができる。
plural [_] = False
-- plural (_ : []) = False
plural _ = True

-- plural [] = True
-- plural (_ : xs) = not $ null xs

myNull :: [a] -> Bool
myNull [] = True
-- myNull (_ : _) = False
myNull _ = False

myTail :: [a] -> [a]
myTail (_ : xs) = xs

myHead :: [a] -> a
myHead (x : _) = x
myHead _ = error "Bonehead!"
