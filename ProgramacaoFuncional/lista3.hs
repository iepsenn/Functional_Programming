--exercicio 1 
adicionaTupla :: (Int, Int) -> Int 
adicionaTupla (a, b) = a + b

--exercicio 2 
shift :: ((Int, Int), Int) -> (Int, (Int, Int))
shift ((a,b),c) = (a,(b,c))

--exercicio 3
minEmax :: Int -> Int -> Int -> (Int, Int)
minEmax a b c = ((my_max a b c), (my_min a b c ))

my_min :: Int -> Int -> Int -> Int
my_min a b c 
	| (a<=b)&&(a<=c) = a
	| (b<=a)&&(b<=c) = b
	| otherwise = c

my_max :: Int -> Int -> Int -> Int
my_max a b c 
	| (a>=b)&&(a>=c) = a
	| (b>=a)&&(b>=c) = b
	| otherwise = c

--exercicio 4 
ordenaTupla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTupla (a, b, c) = ((my_min a b c), (mid a b c) , (my_max a b c ))

mid :: Int -> Int -> Int -> Int
mid a b c 
	| (my_max a b c == a)&&(my_min a b c == c) = b 
	| (my_max a b c == b)&&(my_min a b c == c) = a
	| otherwise = c 

--exercicio 5 
zeroVenda :: Int -> (Int, Bool)
zeroVenda n
	| vendas n == 0 = (n, True) 
	| otherwise = (-1, False)

vendas :: Int -> Int 
vendas 0 = 1 
vendas 1 = 22
vendas 2 = 1
vendas 3 = 8 
vendas 4 = 10
vendas 5 = 0
vendas 6 = 20
vendas _ = 30

--exercicio 6 
-- titulo, autor, isbn
type Autor = String	
type Titulo = String
type Isbn = Int
type Livro = (Titulo, Autor, Isbn)

--livro :: Livro
--livro = ("hue","lukas",12)

titulo :: Livro -> Titulo
titulo (t, a, i) = t

autor :: Livro -> Autor
autor (t, a, i) = a

isbn :: Livro -> Isbn
isbn (t, a, i) = i
