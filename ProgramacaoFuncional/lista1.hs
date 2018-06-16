--exercicio 1 
osQuatroSaoIguais :: Int -> Int -> Int -> Int -> Bool
osQuatroSaoIguais a b c d = (a==b)&&(a==c)&&(a==d)

--exercicio 2 
quantosSaoIguais :: Int -> Int -> Int -> Int 
quantosSaoIguais a b c  
	| (a==b)&&(b==c) = 3
	| (a==b) || (b==c) || (a==c) = 2
	| otherwise = 0

--exercicio 3 
todosDiferentes :: Int -> Int -> Int -> Bool 
todosDiferentes a b c = (a /= b) && (b/=c) && (a/=c)

--exercicio 6 
todosIguais :: Int -> Int -> Int -> Bool
todosIguais a b c = (a==b) && (b==c) && (a==c)

--exercicio 7 
quantosSaoIguais2 :: Int -> Int -> Int -> Int 
quantosSaoIguais2 a b c 
	| todosIguais a b c = 3 
	| todosDiferentes a b c = 0
	| otherwise = 2 

--exercicio 8 
elevaDois :: Int -> Int 
elevaDois n = n*n

--exercicio 9 
elevaQuatro :: Int -> Int 
elevaQuatro n = elevaDois(elevaDois n)

--exercicio 10
vendas :: Int -> Int 
vendas 0 = 1 
vendas 1 = 2 
vendas 2 = 4
vendas 3 = 8 
vendas _ = 10

vendaTotal :: Int -> Int
vendaTotal 0 = vendas 0 
vendaTotal n = vendas n + vendaTotal (n-1)






