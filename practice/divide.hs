const x _ = x

myDivide :: Double -> Double -> Double
myDivide 0 = const 0
myDivide a = (/ a)
