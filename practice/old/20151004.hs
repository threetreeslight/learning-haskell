-- Haskell Revision
import Data.List
import Data.Char

-- whereのありがたみをちゃんと理解できていない
-- Pattern matchの基礎であるMaybe型をしっかりやり直す
-- folrの考え方をやり直す

-- https://skami.iocikun.jp/computer/haskell/web_lecture/for_programmer/repeat_def.html

-- ----------------------------------------
-- [NEW] ++, concat, reverse
-- ----------------------------------------

-- 型で演算子化して定義している
(.++), (.++.), (.++..) :: [a] -> [a] -> [a]

-- ２項演算子としての定義
(x : xs) .++ ys = x : (xs .++ ys)
[] .++ ys = ys

-- 初期値をysとして処理、引数xsを当たえる
-- xs .++. ys = foldr (:) ys xs
-- (.++.) xs ys = foldr (:) ys xs
-- (.++.) xs ys = flip (foldr (:)) xs ys
-- (.++.) = flip (foldr (:))
(.++.) = flip $ foldr (:)

(.++..) = curry . unfoldr $ \xys -> case xys of
        (x : xs, ys) -> Just (x, (xs, ys))
        (_, y : ys) -> Just (y, ([], ys))
        _ -> Nothing

concatRaw, concatF :: [[a]] -> [a]
concatRaw (xs : xss) = xs ++ concatRaw xss
concatRaw _ = []

concatF = foldr (++) []

-- whereがちゃんと理解できていない
-- rsの頭にxを詰めていく
reverseRaw, reverseF :: [a] -> [a]
reverseRaw = rv []
        where
        rv rs (x : xs) = rv (x : rs) xs
        rv rs _ = rs

-- flip (:) (flip (:) [3,4] 2) 1
-- flip (:) xs みたいにして xを頭にどんどん足せるようにする
reverseF = foldl (flip (:)) []

-- ----------------------------------------
-- zip, zipWith, unzip
-- ----------------------------------------

zipRaw, zipU :: [a] -> [b] -> [(a,b)]
zipRaw (x : xs) (y : ys) = (x, y) : zipRaw xs ys
zipRaw _ _ = []

zipU = curry . unfoldr $ \l -> case l of
        (x : xs, y : ys) -> Just ((x,y), (xs, ys))
        _ -> Nothing

unzipRaw, unzipF :: [(a,b)] -> ([a], [b])
unzipRaw ((x, y) : xys) = (x : xs, y : ys)
        where (xs, ys) = unzipRaw xys
unzipRaw _ = ([], [])

unzipF = foldr (\(x, y) (xs, ys) -> (x : xs, y : ys)) ([], [])
-- なんでwhere句分が折り畳まれるんだろう変数としては一つだから？？


-- ----------------------------------------
-- filter define by unfoldr and dropWhile
-- ----------------------------------------

filterU :: (a -> Bool) -> [a] -> [a]
filterU p = unfoldr $ \l -> case dropWhile (not . p) l of
        x : xs -> Just (x, xs)
        _ -> Nothing

-- ----------------------------------------
-- takeWhile, dropwhile, span
-- ----------------------------------------

takeWhileRaw, takeWhileF :: (a -> Bool) -> [a] -> [a]
takeWhileRaw p ( x : xs ) | p x = x : takeWhileRaw p xs
takeWhileRaw _ _ = []

takeWhileF p = foldr (\x -> if p x then (x :) else const [] ) []

dropWhileRaw :: (a -> Bool) -> [a] -> [a]
dropWhileRaw p (x : xs) | p x = dropWhileRaw p xs
dropWhileRaw _ xs = xs

spanRaw :: (a -> Bool) -> [a] -> ([a], [a])
spanRaw p (x : xs) | p x = (x : t, d)
        where (t,d) = spanRaw p xs
spanRaw _ xs = ([], xs)

-- ----------------------------------------
-- take, drop, splitAt
-- ----------------------------------------

takeRaw, takeU :: Int -> [a] -> [a]
takeRaw n (x : xs) | n > 0 = x : takeRaw (n - 1) xs
takeRaw _ _ = []

-- curry化することで unfoldrからのreturnを Just (x, (n, xs))
takeU = curry . unfoldr $ \nl -> case nl of
        (n, x : xs) | n > 0 -> Just (x, (n-1, xs))
        _ -> Nothing

dropRaw  :: Int -> [a] -> [a]
dropRaw n (x : xs) | n > 0 = dropRaw (n - 1) xs
dropRaw _ xs = xs

splitAtRaw :: Int -> [a] -> ([a], [a])
splitAtRaw n (x : xs) | n > 0 = (x : t, d)
          where (t,d) = splitAtRaw (n - 1) xs
splitAtRaw _ xs = ([], xs)


-- ----------------------------------------
-- filter, partition
-- ----------------------------------------

filterRaw, filterF :: (a -> Bool) -> [a] -> [a]

filterRaw p (x : xs)
        | p x = x : filterRaw p xs
        | otherwise = filterRaw p xs
filterRaw _ _ = []

filterF p = foldr (\x -> if p x then (x : ) else id) []

partitionRaw, partitionF :: (a -> Bool) -> [a] -> ([a], [a])

partitionRaw p (x : xs)
        | p x = (x : ts, es)
        | otherwise = (ts, x : es)
        where (ts, es) = partitionRaw p xs
partitionRaw _ _ = ([], [])

partitionF p = foldr (\x (ts, es) -> if p x then (x :ts, es) else (ts, x : es)) ([], [])

-- ----------------------------------------
-- map
-- ----------------------------------------

mapRaw, mapF, mapU :: (a -> b) -> [a] -> [b]

mapRaw f (x : xs) = f x : mapRaw f xs
mapRaw _ _ = []

-- mapF f = foldr (\x xs -> f x : xs) []
mapF f = foldr ((:). f) []
mapU f = unfoldr $ \l -> case l of
        x : xs -> Just (f x, xs)
        _ -> Nothing


-- ----------------------------------------
-- fibonacci
-- ----------------------------------------

fibs, tfibs :: [Integer]
fibs@(_ : tfibs) = 0 : 1 : zipWith (+) fibs tfibs

-- ----------------------------------------
-- @ pattern
-- ----------------------------------------

dupHead :: [a] -> [a]
-- depHead (x : xs) = x : x : xs
-- dupHead _ = []

dupHead xa@(x : _) = x : xa

-- ----------------------------------------
-- define takeTo by unfoldr
-- ----------------------------------------

takeTo' :: (a -> Bool) -> [a] -> [a]
-- takeTo' p [] = []
-- takeTo' p (x : xs)
--         | p x = [x]
--         | otherwise x : takeTo p xs

-- takeTo' p = foldr (\x -> (x :). if p x then const [] else id) []

takeTo' p = unfoldr $ \s -> case s of
        [] -> Nothing
        x : xs  | p x -> Just (x, [])
                | otherwise -> Just (x, xs)


-- ----------------------------------------
-- definition of unfoldr
-- ----------------------------------------

factorization' :: Integer -> [Integer]
factorization' n = myUnfoldr popFactor n

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f s = case f s of
        Nothing -> []
        Just (x, s') -> x : myUnfoldr f s'

pop10 :: Integer -> Maybe (Integer, Integer)
pop10 n = case n `mod` 10 of 
        0 -> Nothing
        _ -> Just (n `mod` 10, n `div` 10)

pop10List :: Integer -> [Integer]
-- pop10List n = case pop10 n of 
--         Nothing -> [] 
--         Just (x, n') -> x : pop10List n'

pop10List = unfoldr pop10



-- ----------------------------------------
-- factorization
-- ----------------------------------------

-- head $ filter ((== 0) . (n `mod`)) [2 ..]

popFactor :: Integer -> Maybe (Integer, Integer)
popFactor n | n < 2 = Nothing
popFactor n = Just (f , n `div` f)
        where f = head $ filter ((== 0) . (n `mod`)) [2 ..]

-- factorization :: Integer -> [Integer]
-- factorization n = case popFactor n of
--         Nothing -> []
--         Just (f, n') -> f : factorization n'

factorization = unfoldr popFactor

-- ----------------------------------------
-- iterate definition
-- ----------------------------------------

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

itrCond :: Integer -> Integer
itrCond n = case n `mod` 3 of
        0 -> n `div` 3
        1 -> n * 2 + 1
        2 -> n * 2 + 2

itrCondInf :: Integer -> [Integer]
itrCondInf = iterate itrCond

-- ----------------------------------------
-- collats
-- ----------------------------------------

takeTo :: (a -> Bool) -> [a] -> [a]
-- takeTo _ [] = []
-- takeTo p (x : xs)
--         | p x = [x]
--         | otherwise = x : takeTo p xs

-- fun p x lst = if p x then [x] else x : lst
fun p x = (x :) . if p x then const [] else id
-- takeTo p (x : xs) = fun p x (takeTo p xs)
-- takeTo p (x : xs) = p x `fun` takeTo p xs
takeTo p = foldr (\x -> (x :) . if p x then const [] else id) []

collatzNext :: Integer -> Integer
collatzNext n
        | even n = n `div` 2
        | otherwise = n * 3 + 1

collatzInf :: Integer -> [Integer]
collatzInf n = n : collatzInf (collatzNext n)

-- ----------------------------------------
-- enum integer
-- ----------------------------------------

myEnumFromTo :: Integer -> Integer -> [Integer]

myEnumFromTo m n | m > n = []
myEnumFromTo m n = m : myEnumFromTo (m + 1) n

