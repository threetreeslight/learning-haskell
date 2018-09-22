import Data.Char
import System.Random
import Control.Arrow

reachable :: Tree -> Char -> Char -> Bool
reachable _ s e | s == e = True
reachable t s e = case lookup s t of
        Nothing -> False
        Just (l, r) -> reachable t l e || reachable t r e



type Tree = [(Char, (Char, Char))]
sampleTree :: Tree
sampleTree = [('a', ('b', 'c')), ('b', ('d', 'e')), ('e', ('f', 'g'))]

sumN :: Integer -> Integer
sumN 0 = 0
sumN n = sumN (n - 1) + n
-- sumN = (sumN (`-` 1))

-- estPi n = (4 *) . sum . take n $ map (\k -> (-1) ** k /(2 * k + 1)) [0..]
estPi = (4 *) . sum . (`take` map (\k -> (-1) ** k /(2 * k + 1)) [0..])

-- estPi4 0 = 1
-- estPi4 n = estPi4 (n - 1) + (-1) ** n / (2 * n + 1)
-- estPi :: Double -> Double
-- estPi = (4 *) . estPi4


oneToFive :: Integer -> Integer
oneToFive x = case x `mod` 5 of
      0 -> 5
      d -> d

diffRecip :: Double -> Double -> Maybe Double
diffRecip x y = case x - y of
      0 -> Nothing
      d       | d > 0  -> Just $ recip d
              | otherwise  -> Just $ recip (- d)

checkAnser :: Char -> Maybe Bool
checkAnser c = case toLower c of
        'y' -> Just True
        'n' -> Just False
        _ -> Nothing


safeRecip = \x  -> case x of
        0 -> Nothing
        _ -> Just $ 1 / x

-- safeRecip 0 = Nothing
-- safeRecip x = Just $ 1 / x

v1 = x ^ x
        where
        x = 5
v2 = x * y
        where
        x = 3
        y = 5

guessPi :: Int -> Int -> Double
guessPi g n = 4 * fromIntegral (length $ inCirclePoints g n) / fromIntegral n

inCirclePoints :: Int -> Int -> [(Double, Double)]
inCirclePoints  g n = filter inCircle . take n $ points g

inCircle :: (Double, Double) -> Bool
inCircle (x,y) = x ^ 2 + y ^ 2 <= 1

points :: Int -> [(Double, Double)]
points = uncurry zip . (randomRs(-1, 1) *** randomRs(-1, 1)) . split . mkStdGen

-- take 10 . uncurry zip . (randomRs (-1, 1) *** randomRs (-1, 1)) . split $ mkStdGen 8 :: [(Double , Double )]

cubes :: [Integer]
cubes = map (^3) [0..]

squares :: [Integer]
squares = map (^2) [0..]

factor n
        | n < 2 = 1
        | otherwise = head $ filter ((== 0) . (n `mod`)) [2..]

productOdds :: Integer -> Integer
productOdds n = product $ filter ((/= 0) . (`mod` 3)) $ filter odd [1..(2*n+1)]


sum3N5 :: Integer -> Integer
sum3N5 n = sum . map (* 3) $ filter ((/= 0) . (`mod` 5)) [0..n]

sum3N :: Integer -> Integer
sum3N n = sum $ map (* 3) [0..n]

-- productOdds :: Integer -> Integer
-- productOdds n = product $ filter odd [1..(2*n+1)]

-- sum3N :: Integer -> Integer
-- sum3N n = sum $ filter ((== 0) . (`mod` 3)) [0 ..3*n]
-- -- x `mod` 3 == 0


productN :: Integer -> Integer
productN n = product [1..n]

-- sumN :: Integer -> Integer
-- sumN n = sum [0..n]

uncurry3 :: (a -> b -> c -> d ) -> (a,b,c) -> d
uncurry3 f (a, b, c) = f a b c

congruent :: Integer -> Integer -> Integer -> Bool
congruent x y z = y `mod` x == z `mod` x


tax' :: (Integer, Integer) -> Integer
tax' = uncurry tax

tax :: Integer -> Integer -> Integer
tax p t = p + p * t `div` 100

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)

triangle :: (Integer, Integer, Integer) -> Bool
triangle (x, y, z) =
        x >= 1
        && y >= 1
        && z >= 1
        && x < (y + z)
        && y < (x + z)
        && z < (x + y)


publish' = curry publish 39

publish :: (Integer, Bool) -> String
publish (x, True) = show x
publish (x, False) = "secret"

introduction' :: Integer -> String
introduction' = curry introduction "Akira"

introduction :: (String, Integer) -> String
introduction (n, a) =
  "My name is " ++ n ++
  ". I'm " ++ show a ++ " years old."


convert :: (Double -> Double) -> Integer -> Double
-- convert f n = f $ fromIntegral n
-- convert = f . fromIntegral
convert = (. fromIntegral)

half :: Double -> Double
half = (/ 2)

seven :: Integer
seven = 7

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x,y) (p,q) = sqrt $ (p - x) ^ 2 + (q - y) ^2

dist0' :: (Double, Double) -> Double
dist0' (x, y) = dist0 x y

px, py :: Double
px = 9
py = 5

dist0 :: Double -> Double -> Double
dist0 x y = sqrt $ x ^ 2 + y ^ 2

-- fun c = (even . (`mod` 7) ) toLower c
-- fun c = even $ (`mod` 7) $ ord $ toLower c
fun c = even . (`mod` 7) . ord $ toLower c

