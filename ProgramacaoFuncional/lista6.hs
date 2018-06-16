--exercicio 1 
somaTripla :: [(Int, Int, Int)] -> Int
somaTripla [] = 0
somaTripla [(a,b,c)] = a+b+c 

--exercicio 2 
somaTupla :: [((Int, Int), (Int, Int))] -> Int
somaTupla [] = 0
somaTupla [((a,b),(c,d))] = a+b+c+d

--exercicio 3 
zippy :: [Int] -> [Int] -> [(Int, Int)]
zippy [] _ = []
zippy _ [] = []
zippy (a:xa) (b:xb) = (a, b) : zippy xa xb

--exercicio 5 
unZippy :: [(Int, Int)] -> ([Int], [Int])
unZippy [] = ([],[])
unZippy l = (unZippyEsq l, unZippyDir l)

unZippyEsq :: [(Int, Int)] -> [Int]
unZippyEsq [] = []
unZippyEsq ((a,b):x) = a : unZippyEsq x

unZippyDir :: [(Int, Int)] -> [Int]
unZippyDir [] = []
unZippyDir ((a,b):x) = b : unZippyDir x