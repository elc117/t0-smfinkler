import Data.Char
main = do 
  putStrLn "Vamos testar as funcoes abaixo!"



---------------------------------------------------------------



--exercicio 1 - FUNÇÃO PRINCIPAL
isBin :: String -> Bool
isBin "" = True
isBin (x:xs) 
  |x=='0' = isBin xs
  |x=='1' = isBin xs
  |otherwise = False



---------------------------------------------------------------



--exercicio 2 - FUNÇÃO PRINCIPAL
isBin2 :: String -> Bool
isBin2 str = if (filter (not . (\c->elem c "01")) str) == "" then True else False



-------------------------------------------------------------



--aux exe 3
auxBin2Dec :: [Int] -> Int -> Int
auxBin2Dec [] ex = 0
auxBin2Dec (x:xs) ex = x*2^ex + auxBin2Dec xs (ex-1)

--exercicio 3 - FUNÇÃO PRINCIPAL
bin2dec :: [Int] -> Int
bin2dec [] = undefined
bin2dec bits = auxBin2Dec bits ((length bits)-1)



-------------------------------------------------------------



--auxaux exe4
preencheExpoentes :: Int -> [Int]
preencheExpoentes tam = [ x | x<- [tam-1, tam-2..0]]

--auxaux exe4
multiplicar :: [Int] -> [Int]
multiplicar lst = map(\x->x*2) lst

--aux exe4
aux :: [Int] -> Int -> Int
aux lst tam = sum([ x | x<- (zipWith(^) (multiplicar lst) (preencheExpoentes tam))])

--exercicio 4 - FUNÇÃO PRINCIPAL
bin2dec2 :: [Int] -> Int
bin2dec2 x = aux x (length x)



-------------------------------------------------------------



--aux exe5
auxDec2Bin :: Int -> [Int] -> [Int]
auxDec2Bin 0 (x:xs) = []
auxDec2Bin num (x:xs) = (mod num 2) : auxDec2Bin (div num 2) (x:xs)

-- exercicio 5 - FUNÇÃO PRINCIPAL
dec2bin :: Int -> [Int]
dec2bin num = reverse(auxDec2Bin num [0])



------------------------------------------------------------


--auxaux exe 6
encodeMe :: Char -> Int
encodeMe c 
   | c == '0'  = 0
   | c == '1'  = 1
   | c == '2'  = 2
   | c == '3'  = 3
   | c == '4'  = 4
   | c == '5'  = 5
   | c == '6'  = 6
   | c == '7'  = 7
   | c == '8'  = 8
   | c == '9'  = 9
   | c == 'A'  = 10
   | c == 'B'  = 11
   | c == 'C'  = 12
   | c == 'D'  = 13
   | c == 'E'  = 14
   | c == 'F'  = 15
   | otherwise = undefined

--aux exe 6
converter :: String -> [Int]
converter str = map encodeMe str

--aux exe 6
auxHex2Dec :: [Int] -> Int -> Int
auxHex2Dec [] ex = 0
auxHex2Dec (x:xs) ex = x*16^ex + auxHex2Dec xs (ex-1)

--exercicio 6 - FUNÇÃO PRINCIPAL
hex2dec :: String -> Int
hex2dec "" = undefined
hex2dec bits = auxHex2Dec (converter(bits)) ((length bits)-1)



----------------------------------------------------------------