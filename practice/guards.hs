-- safeSqrt x = sqrtBool (x >= 0) x
-- sqrtBool True x = Just (sqrt x)
-- sqrtBool False _  = Nothing

safeSqrt x
  | x >= 0 = Just (sqrt 0)
  | otherwise = Nothing
