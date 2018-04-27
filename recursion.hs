import Text.Show

-- EJEMPLO 1
length' :: [a] -> Int
length' [] = 0 
length' (_:xs) = 1 + length' xs 

reverse' :: [a] -> [a]  
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x] 

-- EJEMPLO 2
countdown :: (Eq t, Num t, Show t) => t -> [Char]
{- Eq para matchear con el 0, Num para hacer la resta, Show para mostrar -}
countdown 0 = "0"
countdown value = show value ++ " " ++ countdown (value - 1)

-- EJEMPLO 3
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)

-- EJERCICIOS
sum' :: Num a => [a] -> a
sum' [] = 0 
sum' (x:xs) = x + sum' xs

elem' :: Eq a => a -> [a] -> Bool
elem' val [] = False
elem' val (x:xs) = val == x || elem' val xs

any' :: (a -> Bool) -> [a] -> Bool
any' cond [] = False
any' cond (x:xs) = cond x || any' cond xs

map' :: (a -> b) -> [a] -> [b]
map' function [] = []
map' function (x:xs) = function x : map' function xs

last' ::[a] -> a
last' [x] = x
last' (x:xs) = last' xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' predicate [] = []
filter' predicate (x:xs)
  | predicate x = x : filter' predicate xs
  | otherwise = filter' predicate xs

-- OTRO EJERCICIO
reemplazarEnN :: Int -> a -> [a] -> [a]
reemplazarEnN 0 val (_:xs) = val:xs
reemplazarEnN pos val (x:xs) = x: reemplazarEnN (pos - 1) val xs