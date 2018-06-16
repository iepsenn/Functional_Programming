--exercicio 1 
vendas :: Int -> Int
vendas 0 = 1
vendas 1 = 2 
vendas 2 = 4
vendas 3 = 8 
vendas _ = 10

vendaTotal :: Int -> Int 
vendaTotal 0 = vendas 0
vendaTotal n = vendas n + vendaTotal (n-1)

total :: (Int -> Int) -> Int -> Int 
total f n = f n 

--exercicio 2 
foldInt :: (Int -> Int -> Int) -> [Int] -> Int
foldInt f [] = 0
foldInt f (x:xs) = f x (foldInt f xs) 

soma :: Int -> Int -> Int
soma x y = x + y

--exercicio 4
filterString :: (Char -> Bool) -> [Char] -> [Char]
filterString f [] = []
filterString f (x:xs)
	| f x = x : filterString f xs
	| otherwise = filterString f xs 

naoEspaco :: Char -> Bool	
naoEspaco x = x /= ' '

--exercicio 6 
somaQuadradoLista :: (Int -> Int) -> [Int] -> Int
somaQuadradoLista f [] = 0
somaQuadradoLista f l =  somaLista (mapInt f l)

quadrado :: Int -> Int 
quadrado n = n*n

mapInt :: (Int -> Int) -> [Int] -> [Int]
mapInt f [] = []
mapInt f (x:xs) = f x : mapInt f xs

somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs 

--exercicio 8 
duasVezes :: (Int -> Int) -> Int -> Int
duasVezes f n = f(f n)