olaMundo :: String
olaMundo = "Alo mundo!"



calcuCirc :: Float->Float
calcuCirc x = x*x * 3.14



calcularJuros :: Float->Float->Float->Float
calcularJuros c i t = c*i*t



maxNum :: Float->Float->Float
maxNum x y
	| x>y = x
	| otherwise = y

 
	
verificarNum :: Float->Float
verificarNum x
	| x>0 = 1
	| x<0 = -1      
	| otherwise = 0

 
	
delta :: Float -> Float -> Float -> (Float, Float)
delta a b c
    | ((b^2)- (4 * a * c)) < 0 = error "Nao existem raizes reais"
    | otherwise = ((-b + sqrt((b^2)- (4 * a * c))) / (2 * a), (-b - sqrt((b^2)- (4 * a * c))) / (2 * a))

    
    
fatorial :: Float->Float
fatorial 1 = 1
fatorial x = x * fatorial (x-1)



fibonnacci :: Int -> Int
fibonnacci 0 = 0
fibonnacci 1 = 1
fibonnacci n = fibonnacci(n - 1) + fibonnacci (n - 2)



fibonnacciLista :: Int -> [Int]
fibonnacciLista 0 = 0:[]
fibonnacciLista 1 = 1:[]
fibonnacciLista n = (fibonnacci(n - 1) + fibonnacci (n - 2)):[]




