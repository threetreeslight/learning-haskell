bmi :: Double -> Double -> Double
bmi h w = w / (h / 100) ^2

isObase :: Double -> Double -> Bool
isObase h w = bmi h w >= 25
