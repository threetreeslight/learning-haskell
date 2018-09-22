import Data.Char

myFormMaybe :: a -> Maybe a -> a
myFormMaybe _ (Just x) = x
myFormMaybe d Nothing = d
