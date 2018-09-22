import Data.Char


flip13 f x y z = f z y x

on3 f3 f x y z = f3 (f x) (f y) (f z)

foo x y z = x + y + z

charType = ord . toLower


apply a f = f a

fromMaybe' :: a -> Maybe a -> a
fromMaybe' = (`maybe` id)

const' :: a -> b -> b
const' = const id

-- fromMaybe d f x -> f x -> 

devide' :: Double -> Double -> Double
devide' 0 = const 0
devide' a = (/ a)

bmi :: Double -> Double -> Double
bmi h w = w / (h / 100) ^ 2

isObase :: Double -> Double -> Bool
isObase h w = bmi h w >= 25



div3 x 
        | (x `mod` 3) == 0 = x `div` 3
        | otherwise = x


-- sqrt' x
--         | (x < 0) = Nothing
--         | otherwise = Just (sqrt x)

-- = sqrtBool (x < 0) x
-- sqrtBool True _ = Nothing
-- sqrtBool _ x  = Just (sqrt x)


friend (Just x) = x ++ "is my friend."
friend Nothing = "I'm alone."

notZero 0 = Nothing
notZero x = Just x

safeRecip 0 = Nothing
safeRecip x = Just(1/x)

helloTo "Yoshikuni" = "Good morning, sir."
helloTo n = "Hello, " ++ n ++ "!"

nor a b = not ( a || b )

isLarge = (> 100)

nary n a b = n*a + b
octal = nary 8
-- octal a b = nary 8 a b

(+%) p t = p + p * t `div` 100
-- tax = \o -> o + o * 8 `div` 100

-- sec h m s = h *60^2 + m * 60 + s
-- sec = \h m s -> h *60^2 + m * 60 + s
sec = \h -> \m -> \s -> h *60^2 + m * 60 + s

twice f x = f ( f x )
triple f x = f ( f ( f x ) )
