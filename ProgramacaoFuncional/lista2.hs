--exercicio 1 
my_max :: Int -> Int -> Int 
my_max a b 
	| (a >= b) = a
	| otherwise = b

--exercicio 2 
vendas :: Int -> Int 
vendas 0 = 1 
vendas 1 = 22
vendas 2 = 1
vendas 3 = 8 
vendas 4 = 10
vendas 5 = 1
vendas 6 = 20
vendas _ = 30


maiorVenda :: Int -> Int
maiorVenda 0 = vendas 0 
maiorVenda n = my_max (vendas n) (maiorVenda (n-1))

--exercicio 3 
maxVenda :: Int -> Int 
maxVenda 0 = vendas 0 
maxVenda n = my_max (maiorVenda n) (maxVenda (n-1))

--exercicio 4
zeroVendas :: Int -> Int 
zeroVendas 0
	| vendas 0 == 0 = 0
	| otherwise = -1
zeroVendas n 
 	| vendas n == 0 = n
 	| otherwise = zeroVendas (n-1)

--exercicio 5 
vendaIgualS :: Int -> Int -> Int
vendaIgualS s 0
	| vendas 0 == s = 0
	| otherwise = -1
vendaIgualS s n 
 	| vendas n == s = n
 	| otherwise = vendaIgualS s (n-1)

--exercicio 8
fatorial :: Int -> Int 
fatorial n
	| n < 2 = 1
	| otherwise = n * fatorial (n-1)

--exercicio 9 
produto :: Int -> Int -> Int
produto a b = a*b

--exercicio 10 
fib :: Int -> Int 
fib 0 = 1 
fib 1 = 1 
fib n = fib(n-1) + fib(n-2)



