module Tarefa3 where
import Data.List
import Types
import Tarefa1

-- | Compacta um Labirinto horizontalmente e verticalmente
compactMaze :: Maze -> Instructions
compactMaze l = replace2 (cm l)

-- | Compacta um Labirinto verticalmente 
replace2 :: Instructions -> Instructions
replace2 l = [decide x l | x <- [0..(length l)-1]] -- pega numa lista de indices

-- | Auxiliar de replace 2 , Decide se devolve um Repeat ou o Instruct
decide :: Int -> Instructions -> Instruction 
decide x l = if k < x -- se o indice for menor quer dizer que é repetido 
             then Repeat k
             else l !! x
    where k = (elemIndices (l!!x) l) !! 0 -- retira o primeiro elemento da lista de elemIndices do elemento com indice x


-- | Compacta horizontalmente o Labirinto
cm :: Maze -> Instructions -- (compacta para todo o labirinto)
cm [] = [] 
cm (h:t)= corredor2 h: cm t

-- | Compacta horizontalmente um Corredor
corredor2 :: Corridor -> Instruction
corredor2 [] = Instruct []
corredor2 l  = Instruct ((contas,head l) :l2)
   where contas = conta l
         l2 = aux(  corredor2 (drop contas l ))
         aux (Instruct l) = l -- ignora o instruct e pega na lista

-- | Conta o número de peças seguidas num corredor
conta :: Corridor -> Int
conta [a] = 1
conta (h:ht:t) = if h == ht then 1 + conta (ht:t)
                 else 1

