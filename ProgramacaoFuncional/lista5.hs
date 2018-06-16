--exercicio 1 
membro :: [Int] -> Int -> Bool
membro [] _ = False
membro (x:xs) n
	| x==n = True 
	| otherwise = membro xs n

--exercicio 2 
membroNum :: [Int] -> Int -> Int
membroNum [] _ = 0
membroNum (x:xs) n 
	| x==n = 1 + membroNum xs n
	| otherwise = membroNum xs n 

--exercicio 4
unico :: [Int] -> [Int]
unico [] = []
unico (x:xs)
	| membroNum (x:xs) x == 1 = x : unico xs 
	| otherwise = unico (removeEl xs x)

removeEl :: [Int] -> Int -> [Int]
removeEl [] _ = []
removeEl (x:xs) n 
	| x==n = removeEl xs n
	| otherwise = x : removeEl xs n  


 

