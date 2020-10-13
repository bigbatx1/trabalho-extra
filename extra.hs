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


