
{- | 

= INTRODUÇÃO

Esta tarefa consistia em gerar um labirinto.

= OBJETIVO

O objetivo desta tarefa é implementar um mecanismo de geração de labirintos.
Fizemos uma método de geração que permite construir um túnel e colocar parede a toda a volta.


= CONCLUSÃO
Conseguimos fazer o proposto de forma eficiente.

-}

module Tarefa1 where
import System.Random
import Types


-- | Dando uma seed devolve uma lista de n integer gerados aleatóriamente
geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = let gen = mkStdGen seed -- cria um gerador aleatório 
                        in take n $ randomRs (0,99) gen -- tira os primeiros n elementos de uma série infinita de números aleatorios entre 0-99


-- | Dando uma seed devolve um integer gerado aleatóriamente
nrAleatorio :: Int -> Int
nrAleatorio seed = head $ geraAleatorios 1 seed


-- | Converte uma lista numa lista de listas de tamanho n
subLista :: Int -> [a] -> [[a]]
subLista _ [] = []
subLista n l = take n l: subLista n (drop n l)


-- | Converte um número numa peça
convertePiece :: Int -> Piece
convertePiece x 
    | x == 3 = Food Big
    | x == 8 = Empty 
    | 0 <= x && x < 70 = Food Little  
    | 70 <= x && x <= 99 = Wall
 

-- | Converte uma lista de integers num corredor
converteCorridor :: [Int] -> Corridor
converteCorridor l = map convertePiece l


-- | Converte uma lista de listas de Integers num Labirinto 
converteLabirinto :: [[Int]] -> Maze
converteLabirinto l = map (\c -> converteCorridor c) l


-- | Converte um corredor numa string
printCorridor :: Corridor -> String
printCorridor l = foldr (\x a -> show x ++ a) "\n" l


-- | Controi uma parede por cima e por baixo de um Labirinto
parcima :: [[Int]] -> [[Int]] 
parcima l@(h:t) = fazWall ta:(init t) ++ [fazWall ta]
    where ta = length h -- ta = tamanho de um Corredor


-- | Controi uma parede ou um túnel no início e no fim de cada corredor
parfim :: [[Int]] -> [[Int]]
parfim [] = []
parfim l = parfimaux l ta 
    where ta = length l -- número de Corredores


-- | Auxiliar de parfim
parfimaux :: [[Int]] -> Int -> [[Int]]
parfimaux [] ta = []
parfimaux l@(h:t) ta 
    | odd ta  = if length l == metade+1 
                then (8:tiral++[8]):par
                else (71:tiral++[71]):par
    | even ta = if length l == metade || length l == metade+1 
                then (8:tiral++[8]):par
                else (71:tiral++[71]):par
    where metade = (div ta 2)
          par = parfimaux t ta
          tiral = (tail$init h)


-- | Constroi uma parede horizontal comprimento n, auxiliar de parcima
fazWall :: Int -> [Int]
fazWall x = replicate x 71


-- | Controi um Casa dos Fantasmas
casa :: [[Int]] -> [[Int]]
casa l = casaaux l al 0
    where al = length l -- altura do labirinto


-- | Constroi vários corredor da Casa dos Fantasmas recursivamente 
casaaux :: [[Int]] -> Int ->Int-> [[Int]]
casaaux [] al co = []
casaaux l@(h:t) al co
    | co == 5 = h:casaaux t al co
    | otherwise = if length l <= metade+3 
                  then [colocaCorridor h co]++casaaux t al (co+1) 
                  else h: casaaux t al co
    where metade = (div al 2)


-- | Coloca nesse Corredor a parte correspondente da Casa dos Fantasmas
colocaCorridor :: [Int] -> Int -> [Int]
colocaCorridor [] co = [] 
colocaCorridor l  co = corridoraux l ta co
    where ta = (length l) -- ta = tamanho de um Corredor


-- | Função auxiliar da colocaCorridor
corridoraux :: [Int] -> Int -> Int -> [Int]
corridoraux [] ta co = []
corridoraux l@(h:t) ta co
    | odd ta  = if length l == metade+6 then (cfimpar !! co) ++ (drop 11 l) 
                else h: (corridoraux t ta co)
    | even ta = if length l == metade+5 then (cfpar !! co ) ++ (drop 10 l) 
                else h: (corridoraux t ta co) 
    where metade = (div ta 2)


-- | Casa dos Fantasmas se for Impar
cfimpar :: [[Int]]
cfimpar= [[8,8,8,8,8,8,8,8,8,8,8],
          [8,70,70,70,8,8,8,70,70,70,8],
          [8,70,8,8,8,8,8,8,8,70,8], 
          [8,70,70,70,70,70,70,70,70,70,8],
          [8,8,8,8,8,8,8,8,8,8,8]]


-- | Casa dos Fantasmas se for Par
cfpar :: [[Int]]
cfpar= [[8,8,8,8,8,8,8,8,8,8],
        [8,70,70,70,8,8,70,70,70,8],
        [8,70,8,8,8,8,8,8,70,8], 
        [8,70,70,70,70,70,70,70,70,8],
        [8,8,8,8,8,8,8,8,8,8]]


-- | Controi um Labirinto final
generateMaze :: Int -> Int -> Int -> Maze
generateMaze x y s =
                 let random_nrs = geraAleatorios (x*y) s
                 in converteLabirinto $ parcima $ parfim $ casa $ subLista x random_nrs


geraLabirinto :: Int -> Int -> Int -> IO ()
geraLabirinto x y s =
                 let random_nrs = geraAleatorios (x*y) s
                 in putStrLn $ printMaze $ converteLabirinto $ parcima $ parfim $ casa $ subLista x random_nrs

