--exercicio 1
dobraLista :: [Int] -> [Int]
dobraLista [] = []
dobraLista (x:xs) = x*2 : dobraLista xs 

--exercicio 2 
tamanho :: [Int] -> Int 
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

--exercicio 3
produtoLista :: [Int] -> Int
produtoLista [] = 1
produtoLista (x:xs) = x * produtoLista xs 

--exercicio 4 
andLista :: [Bool] -> Bool 
andLista [] = False
andLista (x:xs) = x && andLista xs 

--exercicio 5 
concatLista :: [[Int]] -> [Int]
concatLista [] = []
concatLista (x:xs) = x ++ concatLista xs 

--exercicio 6 
inverteLista :: [Int] -> [Int]
inverteLista [] = []
inverteLista (x:xs) = inverteLista xs ++ [x]

