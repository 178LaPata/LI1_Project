module Tarefa5 where
import Types
import Tarefa1
import Tarefa2
import Tarefa3
import Data.List



g1= generateMaze 15 15 1


{-
 U  (-1,0 )
 D  (1 ,0 )
 R  (0 ,1 )
 L  (0 ,-1)
-}

-caminho g ((4,4),(4,10),[],0)
verepet :: [Coords] -> Bool
verepet [] = False
verepet (h:t) = if elem h t then True
                else verepet t

 
--minimo :: [(Int , [Coords])]



caminho :: Maze -> (Coords,Coords,[Coords],Int) -> (Int,[Coords])
--              partida,chegada,percorrido,comprimento -> (comprimento, perc)
caminho lab ((xi,yi),(xf,yf),lc,com)
    | (xi,yi) == (xf,yf) = (com,lc)
    | verepet lc         = (n,lc)
    | lab !! xf !! yf == Wall = (n,lc)
    | lab !! xi !! yi == Wall = (n,lc)
    | otherwise = head (sortOn fst [c1,c2,c3,c4])

    where pecaU = lab !! (xi-1) !! (yi  )
          pecaD = lab !! (xi+1) !! (yi  )
          pecaL = lab !! (xi  ) !! (if yi == 0 then lar-1 else yi-1)
          pecaR = lab !! (xi  ) !! (if yi == lar-1 then 0 else yi+1)

          lar = length (head lab)
          n = (length lab) * lar -- caminho maior possivel é área do lab

          c1 = caminho lab ((xi-1,yi  ),(xf,yf),lc++[(xi,yi)],com+1)
          c2 = caminho lab ((xi+1,yi  ),(xf,yf),lc++[(xi,yi)],com+1)
          c3 = caminho lab ((xi  ,if yi == lar-1 then 0 else yi+1),(xf,yf),lc++[(xi,yi)],com+1)
          c4 = caminho lab ((xi  ,if yi == 0 then lar-1 else yi-1),(xf,yf),lc++[(xi,yi)],com+1)
         
