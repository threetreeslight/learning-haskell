maybe0 (Just x) = x
maybe0 Nothing = 0

safeRecip 0 = Nothing
safeRecip x = Just (1 / x)

helloTo "Yoshikuni" = "Good morning, sir."
helloTo n = "Hello, " ++ n ++ "!"
