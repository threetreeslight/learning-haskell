-- 20150920
import Data.Foldable
import Text.Read
import Data.List

-- ここまでやった。
-- https://skami.iocikun.jp/computer/haskell/web_lecture/for_programmer/rev_polish_fold_left.html

-- ----------------------------------------
-- Reverse Polish Notation && folding left
-- ----------------------------------------

rpolish1 :: Maybe [Integer] -> String -> Maybe [Integer]
rpolish1 (Just ns) s = case lookup s operators of
        Just o -> case ns of
                y : x : ns' -> Just $ x `o` y : ns'
                _ -> Nothing
        _ -> maybe Nothing (Just . (: ns)) $ readMaybe s
rpolish1 _ _ = Nothing

rpolishFoldl :: [String] -> Maybe [Integer]
rpolishFoldl = foldl' rpolish1 $ Just []


bolish :: [(Bool, Integer)] -> Integer
bolish [] = 0

-- ----------------------------------------
-- Reverse Polish Notation
-- ----------------------------------------

rpolishIter :: Maybe [Integer] -> [String] -> Maybe [Integer]
rpolishIter mns [] = mns
rpolishIter (Just ns) (s : ss) = case lookup s operators of
        Just o -> case ns of
                y : x : ns' -> rpolishIter (Just $ x `o` y : ns') ss
                _ -> Nothing
        _ -> rpolishIter (maybe Nothing (Just . (:ns)) $ readMaybe s) ss
rpolishIter _ _ = Nothing

rpolish :: [String] -> Maybe [Integer]
rpolish = rpolishIter $ Just []

-- ----------------------------------------
-- Polish Notation && folding right
-- ----------------------------------------

polish1 :: String -> Maybe [Integer] -> Maybe [Integer]
polish1 s (Just ns) = case lookup s operators of
      Just o -> case ns of
              x : y : ns' -> Just $ x `o` y : ns'
      _ -> maybe Nothing (Just . (: ns)) $ readMaybe s
polish1 _ _ = Nothing

polishFoldr :: [String] -> Maybe [Integer]
polishFoldr = foldr polish1 (Just [])

bolish :: [(Bool, Integer)] -> Integer
bolish [] = 0
-- bolish ((op, n) : ns) = if op then n * bolish ns else n + bolish ns
bolish ((op, n) : ns) = (if op then (*) else (+)) n $ bolish ns

bolish1 :: (Bool, Integer) -> Integer -> Integer
bolish1 (op, n) = (if op then (*) else (+)) n

bolishFoldr :: [(Bool, Integer)] -> Integer
bolishFoldr = foldr bolish1 0

-- ----------------------------------------
-- Polish Notation
-- ----------------------------------------

-- readMaybe "3" :: Maybe Int

operators :: [(String, Integer -> Integer -> Integer)]
operators = [("+", (+)), ("-", (-)), ("*", (*)), ("/", div)]

polish :: [String] -> Maybe [Integer]
polish [] = Just []
polish (s : ss) = case lookup s operators of
        Just o -> case polish ss of
                Just (x : y : ns) -> Just $ x `o` y : ns
                _ -> error "foo"
        _ -> case readMaybe s of
                Just n -> maybe Nothing (Just . (n :)) $ polish ss
                _ -> Nothing

-- ----------------------------------------
-- folding left
-- ----------------------------------------

sumIter :: Integer -> [Integer] -> Integer
sumIter s [] = s
sumIter s (x : xs) = sumIter (s + x) xs
mySum = sumIter 0

myFoldl _ s [] = s
myFoldl op s (x : xs) = myFoldl op (s `op` x) xs

-- ----------------------------------------
-- folding right
-- ----------------------------------------

-- mySum :: [Integer] -> Integer
-- mySum = foldr (+) 0

myProduct :: [Integer] -> Integer
myProduct = foldr (*) 1

myLength :: [a] -> Integer
myLength = foldr (const (1 +)) 0

myFoldr _ v [] = v
myFoldr op v (x : xs) = x `op` myFoldr op v xs

-- ----------------------------------------
-- length of list
-- ----------------------------------------

-- myLength :: [a] -> Int
-- myLength [] = 0
-- -- myLength (_ : xs) = 1 + myLength xs
-- myLength a = sum $ map (const 1) a

-- ----------------------------------------
-- time up list
-- ----------------------------------------

-- myProduct :: [Integer] -> Integer
-- myProduct [] = 1
-- myProduct (x : xs) = x * myProduct xs

myMaximum :: [Integer] -> Integer
myMaximum [] = 0
myMaximum (x : xs) = max x $ myMaximum xs


-- ----------------------------------------
-- Sum up list
-- ----------------------------------------

-- mySum :: [Integer] -> Integer
-- mySum [] = 0
-- mySum (x : xs) = x + mySum xs

-- ----------------------------------------
-- List Pattern match
-- ----------------------------------------

myHead :: [a] -> a
myHead (x : _) = x
myHead _ = error "Bonehead!"

myTail :: [a] -> [a]
myTail (_ : xs) = xs

myNull :: [a] -> Bool
myNull [] = True
myNull _ = False

plural :: [a] -> Bool
plural [_] = False
plural _ = True

-- ----------------------------------------
-- 型シノニム
-- ----------------------------------------

type Point = (Double, Double)
-- dist0 :: (Double, Double) -> Double
dist0 :: Point -> Double
dist0 (x, y) = sqrt $ x ^ 2 + y ^ 2

-- inCircle :: (Double, Double) -> Double -> (Double, Double) -> Bool
inCircle :: Point -> Double -> Point -> Bool
inCircle (x0, y0) r (x, y) = (x - x0) ^ 2 + (y - y0) ^ 2 <= r ^ 2

type Questions a = [(a, a)]
type Answers = [Bool]
type Results a = [(Bool, (a, a))]

questions1 :: Questions String
questions1 = [
        ("Elephant", "Giraffe"),
        ("Apple", "Banana"),
        ("TV", "Radio"),
        ("Plane", "Ship"),
        ("Tennis", "Ski") ]

answers1 :: Answers
answers1 = [True, False, True, True, False]

result :: Questions a -> Answers -> Results a
result = flip zip

hates, likes :: Results a -> [a]
likes = map $ \(a,s) -> (if a then fst else snd) s
hates = map $ \(a,s) -> (if a then snd else fst) s


