module MyModule2  where

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

