-- Haskell Revision
import Data.List

-- Pattern matchの基礎であるMaybe型をしっかりやり直す
-- folrの考え方をやり直す

-- ----------------------------------------
-- filter, partition
-- ----------------------------------------

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

