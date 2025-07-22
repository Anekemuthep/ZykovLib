{-# LANGUAGE FlexibleInstances #-}
import Data.List
--import Prelude hiding ((+),(*))

instance Num ([Int], [[Int]]) where
  (+) (xs,ys) (zs,ws) = (xs ++ zs, ys ++ ws)
  (*) (xs,ys) (zs,ws) = (xs ++ zs, ys ++ ws ++ unique [[u,v] | u <- xs, v <- zs])
  abs = undefined
  signum = undefined
  fromInteger = undefined

unique :: Eq a => [[a]] -> [[a]]
unique = filter (\[x, y] -> x /= y)
