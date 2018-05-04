-- EJEMPLO 1
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

-- EJEMPLO 2
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- EJEMPLO 3
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)

-- EJEMPLO 4
repeat' :: a -> [a]
repeat' x = x : repeat' x

-- EJEMPLO 5

{- A -}
reemplazarEn :: Int -> a -> [a] -> [a]
reemplazarEn pos val (x:xs)
  | n == 0 = val : xs
  | otherwise = x : reemplazarEn (pos - 1) val xs
{- B -}
reemplazarEn :: Int -> a -> [a] -> [a]
reemplazarEn pos val lista = 
  take pos lista ++ [val] ++ drop (pos + 1) lista

-- EJERCICIOS
sum' :: Num a => [a] -> a
sum' [] = 0 
sum' (x:xs) = x + sum' xs

any' :: (a -> Bool) -> [a] -> Bool
any' cond [] = False
any' cond (x:xs) = cond x || any' cond xs

map' :: (a -> b) -> [a] -> [b]
map' function [] = []
map' function (x:xs) = function x : map' function xs

elem' :: Eq a => a -> [a] -> Bool
elem' val [] = False
elem' val (x:xs) = val == x || elem' val xs

last' ::[a] -> a
last' [x] = x
last' (x:xs) = last' xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' predicate [] = []
filter' predicate (x:xs)
  | predicate x = x : filter' predicate xs
  | otherwise = filter' predicate xs

