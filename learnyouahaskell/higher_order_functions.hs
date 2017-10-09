-- all functions that took more than paramaters are just curried functions.
-- divide by ten
divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)
-- check if char is an upper alpha
isUpperAlpha :: Char -> Bool
isUpperAlpha = (`elem` ['A'..'Z'])
-- apply a function twice
applyTwice :: (a -> a) -> a -> a
applyTwice f a = f (f a)
-- zipwith
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
-- map
-- map' :: (a -> b) -> [a] -> [b]
-- map' f (x:xs) = f x : map' f xs
-- sum using foldl
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs
-- sum using foldl and even more succinct
sum'' :: (Num a) => [a] -> a
-- (+) is the same as the lambda function above. 
-- we can omit xs because foldl (+) 0 returns a function that takes a list
sum'' = foldl (+) 0
-- implement elem using left fold
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys 
-- foldr
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr(\x acc -> f x : acc) [] xs
-- lets see the power of folds...
-- maximum using foldr1
maximum' :: (Ord a) => [a] -> a
maximum' = foldr1(\x acc -> if acc > x then acc else x)
-- reverse using foldl
reverse' :: [a] -> [a]
reverse' = foldl(\acc x -> x:acc) []
-- product
product' :: (Num a) => [a] -> a
product' = foldl1(*)
-- filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr(\x acc -> if p x then x:acc else acc) []
-- head - better to do this with pattern matching, but here goes...
head' :: [a] -> a
head' = foldr1 (\x _ -> x)
-- last
last' :: [a] -> a
last' = foldl1(\_ x -> x)
-- scanl - how many elements does it take for the sum of the roots to exceed 1k?
sqrtSums :: Int
sqrtSums = length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1..]))) + 1
-- Function application with $
-- ($) :: (a -> b) -> a -> b
-- f $ x = f x
-- function composition: use the . operator.
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g = \x -> f (g x)
