--ex1
  
  
  bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [] = []
bubbleSort lista =
  let troca [x] = [x]
      troca (x : y : xs) =
        if x > y
          then x : troca (y : xs)
          else y : troca (x : xs)

      divideLista lista = (take (length lista - 1) lista, drop (length lista - 1) lista)

      bubble [x] = [x]
      bubble l = (bubble aTrocar) ++ ultimoElmento
        where
          listaModificada = troca l
          (aTrocar, ultimoElmento) = divideLista listaModificada
   in bubble lista
   
   --ex2
    
   removeMaior :: (Ord a) => (a, [a]) -> (a, [a])
removeMaior (m, [x]) =
  if x > m
    then (x, [m])
    else (m, [x])
removeMaior (maior, (x : xs))
  | x > maior = add maior (removeMaior (x, xs))
  | otherwise = add x (removeMaior (maior, xs))
  where
    add a (n, l) = (n, a : l)

selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort [x] = [x]
selectionSort lst =
  let (least, novoUlt) = removeMenor (head lst, tail lst)
   in least : (selectionSort novoUlt)
   
   
   --ex3
   
   divide :: (Ord a) => a -> [a] -> ([a], [a])
divide _ [] = ([], [])
divide x [e] =
  if e > x
    then ([e], [])
    else ([], [e])
divide x (e : es)
  | e > x = addEsq e (divide x es)
  | otherwise = addDir e (divide x es)
  where
    addEsq a (l, r) = (a : l, r)
    addDir a (l, r) = (l, a : r)

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (piv : xs) =
  let (left, right) = divide piv xs
   in (quickSort left) ++ [piv] ++ (quickSort right)

--ex4
--a)exemplo aparentemente errado
somat :: Int -> Int
somat n = foldr (\x y -> x^3 + y) 0 [0..n]

--b)exemplo aparentemente errado
produt :: Int -> Int
produt n = foldr (\x y -> x^3 * y) 1 [1..n]

--c)
pares1 :: [(Int, Int)] -> [Int]
pares1 lst = map (\(x,y) -> x+y) (filter (\(x,y) -> y > x) lst)
   
--d)
pares2 :: [(Int, Int)] -> [Int]
pares2 lst = map (\(x,y) -> x^y) lst

--e)
totaliza1 :: [(Int, Int)] -> Int
totaliza1 lst =  foldr1 (+) (map (\(x,y) -> x*y) (filter (\(x,y) -> y > x) lst))

--f)
totaliza1 :: [(Int, Int)] -> Int
totaliza1 lst =  foldr1 (+) (map (\(x,y) -> x*y) (filter (\(x,y) -> y > x) lst))

--g)
triplas :: [(Int, Int, Int)] -> [Int]
triplas lst = map (\(x,y,z) -> x*y*z) (filter (\(x,y,z) -> x > y && y <z ) lst)

--h)
triplas :: [(Int, Int, Int)] -> [Int]
triplas lst = map (\(x,y,z) -> x+y+z) (filter (\(x,y,z) -> odd(x+y+z) ) lst)

--ex5
data ArvoreBinInt = Nulo | No Int ArvoreBinInt ArvoreBinInt
                    deriving(Show, Eq)


arvEx::ArvoreBinInt
arvEx = (No 2 (No 7 (No 12 Nulo Nulo) (No 6 (No 5 Nulo Nulo) (No 11 Nulo Nulo))) (No 15 Nulo (No 9 (No 4 Nulo Nulo) Nulo)))
buscaPos :: Int -> ArvoreBinInt -> (Bool, Int)
buscaPos elem arv = buscaPosAux elem arv 0

buscaPosAux :: Int -> ArvoreBinInt -> Int -> (Bool, Int)
buscaPosAux _ Nulo cont = (False, cont)
buscaPosAux elem (No info esq dir) cont = 
  let
    (acho1, cont1) = buscaPosAux elem esq (cont)
    (achou2, cont2) = buscaPosAux elem dir (cont1)
    (achou3, cont3) = if info == elem then (True, cont2+1) else (False, cont2+1)
   
  in 
    if acho1 then (acho1, cont1)
    else 
        if achou2 then (achou2, cont2)
            else (achou3, cont3)

--ex6


data ArvoreBinInt = Nulo | No Int ArvoreBinInt ArvoreBinInt
                    deriving(Show, Eq)
arvEx::ArvoreBinInt
arvEx = (No 7 (No 3 (No 1 Nulo Nulo) (No 6 (No 4 Nulo Nulo) Nulo)) (No 10 Nulo (No 14 Nulo Nulo))) 




somapares :: ArvoreBinInt -> (Int, Int)
somapares Nulo = (0,0)
somapares (No info Nulo Nulo) = if even info then (info, 0) else (0,0)
somapares (No info esq dir) = add (somapares esq) (somapares dir)
    where
        add (contEsq1, contEsq2) (contDir1, contDir2) = if even info then (contEsq1+contDir1, contEsq2+contDir2+info) else (contEsq1+contDir1, contEsq2+contDir2)
        
        
        
 --ex7
 type Nome = String
 type Codigo=Int
 type Meses=Int
 type Pagamento = String
 type Volume = String



data TA = Cliente Nome  Codigo Meses Pagamento Volume deriving (Show)

listaClientesTA :: [TA]
listaClientesTA =
     [
        (Cliente "Andre" 2001 10 Dinheiro Baixo),
        (Cliente "arcia" 2001 8 Dinheiro Baixo),
        (Cliente "arcio" 2001 7 Dinheiro Baixo),
        (Cliente "Cleide" 2001 5 Dinheiro Medio),
        (Cliente "dalfo" 2001 6 Dinheiro Medio),
        (Cliente"darcio" 2001 7 Dinheiro Medio),
        (Cliente "dagata" 2001 8 Dinheiro Alto),
        (Cliente "dine" 2001 9 Dinheiro Alto),
        (Cliente "din0" 2001 10 Dinheiro Alto),
        (Cliente "eussa" 2001 5 Cartao Baixo),
        (Cliente "eusso" 2001 7 Cartao Baixo),
        (Cliente "fatima" 2001 8 Cartao Baixo),
        (Cliente "fatimo" 2001 12 Cartao Medio),
        (Cliente "giovana" 2001 10 Cartao Medio),
        (Cliente "giovano" 2001 7 Cartao Medio),
        (Cliente "henri" 2001 8 Cartao Alto),
        (Cliente "henro" 2001 9 Cartao Alto),
        (Cliente "isa" 2001 5 Cartao Alto),
        (Cliente "iso" 2001 8 Boleto Baixo),
        (Cliente "junio" 2001 9 Boleto Baixo),
        (Cliente "junia" 2001 10 Boleto Baixo),
        (Cliente "karlos" 2001 11 Boleto Medio),
        (Cliente "karlas" 2001 10 Boleto Medio),
        (Cliente "lisa" 2001 8 Boleto Medio),
        (Cliente "liso" 2001 6 Boleto Alto),
        (Cliente "marcela" 2001 5 Boleto Alto),
        (Cliente "marcelo" 2001 7 Boleto Alto)
        ]
 
 
