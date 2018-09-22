-- 201509267
import Data.List


-- Next is here
-- https://skami.iocikun.jp/computer/haskell/web_lecture/for_programmer/plusplus_def.html

-- ----------------------------------------
-- zip, zipWith, unzip
-- ----------------------------------------

zipRaw, zipU :: [a] -> [b] -> [(a,b)]

zipRaw (x : xs) (y : ys) = (x,y) : zipRaw xs ys
zipRaw _ _ = []

zipU = curry . unfoldr $ \l -> case l of
        (x : xs, y : ys) -> Just ((x,y), (xs, ys))
        _ -> Nothing

zipWithRaw, zipWithU :: (a -> b -> c) -> [a] -> [b] -> [c]

zipWithRaw op (x : xs) (y : ys) = x `op` y : zipWithRaw op xs ys
zipWithRaw _ _ _ = []

zipWithU op = curry . unfoldr $ \l -> case l of
        (x : xs, y : ys) -> Just (x `op` y, (xs, ys))
        _ -> Nothing

zipZW = zipWith (,)

unzipRaw, unzipF :: [(a, b)] -> ([a], [b])

unzipRaw ((x, y) : xys) = (x : xs, y :ys)
        where (xs, ys) = unzipRaw xys
unzipRaw _ = ([], [])

unzipF = foldr (\(x,y) (xs, ys) -> (x : xs, y : ys)) ([], [])






-- ----------------------------------------
-- Make filter by unfoldr & dropWhile 
-- ----------------------------------------

filterU :: (a -> Bool) -> [a] -> [a]
filterU p = unfoldr $ \l -> case dropWhile (not . p) l of
         x : xs -> Just (x, xs)
         _ -> Nothing

-- ----------------------------------------
-- take drop splitAt
-- ----------------------------------------

takeRaw, takeU :: Int -> [a] -> [a]

takeRaw n (x : xs) | n > 0 = x : takeRaw (n - 1) xs
takeRaw _ _ = []

takeU = curry . unfoldr $ \nl -> case nl of
        (n, x : xs) | n > 0 -> Just (x, (n-1, xs))
        _ -> Nothing

dropRaw :: Int -> [a] -> [a]
dropRaw n (x : xs) | n > 0 = dropRaw (n - 1) xs
dropRaw _ xs = xs

splitAtRaw :: Int -> [a] -> ([a], [a])

splitAtRaw n (x : xs) | n > 0 = (x : t, d)
        where (t, d) = splitAtRaw (n - 1) xs
splitAtRaw _ xs = ([], xs)





-- ----------------------------------------
-- filter
-- ----------------------------------------

filterRaw, filterF :: (a -> Bool) -> [a] -> [a]
filterRaw p (x : xs)
    | p x = x : filterRaw p xs
    | otherwise = filterRaw p xs
filterRaw _ _ = []

filterF p = foldr (\x -> if p x then (x :) else id) []

partitionRaw, partitionF :: (a -> Bool) -> [a] -> ([a], [a])

partitionRaw p (x : xs)
        | p x = (x : ts, es)
        | otherwise = (ts, x : es)
        where (ts, es) = partitionRaw p xs
partitionRaw _ _ = ([], [])

partitionF p = foldr
        (\x (ts, es) -> if p x then (x : ts, es) else (ts, x : es))
        ([], [])

-- ----------------------------------------
-- map
-- ----------------------------------------

mapRaw ,mapF, mapU :: (a -> b) -> [a] -> [b]
-- mapRaw, mapF, mapU :: (a -> b) -> ([a] -> [b])

mapRaw f (x : xs) = f x : mapRaw f xs
mapRaw _ _ = []

mapF f = foldr ((:) . f) []

mapU f = unfoldr $ \l -> case l of
      x : xs -> Just (f x, xs)
      _ -> Nothing

-- ----------------------------------------
-- fibonacci
-- ----------------------------------------

nums, usnums :: [Integer]
-- nums@( _ : usnums) = 0 : 1 : zipWith (+) usnums (map (* 2) nums)
-- nums@( _ : usnums) = 0 : 1 : zipWith (\ a -> ((+) (2 * a))) nums usnums
nums@( _ : usnums) = 0 : 1 : zipWith ((+) . (* 2)) nums usnums



fibs, tfibs :: [Integer]
fibs@( _ : tfibs) = 0 : 1 : zipWith (+) fibs tfibs

ary = 0 : 1 : ary

-- ----------------------------------------
-- @ pattern
-- ----------------------------------------

dupHead :: [a] -> [a]
dupHead (x : xs) = x : x : xs
dupHead _ = []

depHead xa@(x : _) = x : xa


-- ----------------------------------------
-- fst snd
-- ----------------------------------------

myFst :: (a,b) -> a
myFst (f, _) = f

mySnd :: (a,b) -> b
mySnd (_, s) = s

-- ----------------------------------------
-- List : unfolder can be defined by takeTo
-- ----------------------------------------

-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
takeTo :: (a -> Bool) -> [a] -> [a]
-- takeTo p [] = []
-- takeTo p (x : xs)
--         | p x = [x]
--         | otherwise = x : takeTo p xs
-- takeTo p = foldr (\x -> ((x :) . if p x then const [] else id)) []

takeTo p = unfoldr $ \s -> case s of
        [] -> Nothing
        x : xs  | p x -> Just (x, [])
                | otherwise -> Just (x, xs)


-- ----------------------------------------
-- List : unfoldr
-- ----------------------------------------

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f s = case f s of
        Nothing -> []
        Just (x, s') -> x : myUnfoldr f s'

popValue :: Integer -> Maybe (Integer, Integer)
popValue n | n == 0 = Nothing
popValue n = Just ( n `mod` 10 , n `div` 10)

valualization :: Integer -> [Integer]
valualization n = case popValue n of
        Nothing -> []
        Just (f, n') -> f : valualization n'
-- valualization = unfoldr popValue

-- ----------------------------------------
-- List : Factorization
-- ----------------------------------------

popFactor :: Integer -> Maybe (Integer, Integer)
popFactor n | n < 2 = Nothing
popFactor n = Just ( f, n `div` f )
        where f = head $ filter ((== 0) . (n `mod`)) [2..]

factorization :: Integer -> [Integer]
-- factorization n = case popFactor n of
--         Nothing -> []
--         Just (f, n') -> f : factorization n'
factorization = unfoldr popFactor
