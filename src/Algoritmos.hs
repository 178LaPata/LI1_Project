 -- 1º Algoritmo: Testa-se todos os caminhos possíveis

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
          n   = (length lab) * lar -- caminho maior possivel é área do lab

          c1 = caminho lab ((xi-1,yi  ),(xf,yf),lc++[(xi,yi)],com+1)
          c2 = caminho lab ((xi+1,yi  ),(xf,yf),lc++[(xi,yi)],com+1)
          c3 = caminho lab ((xi  ,if yi == lar-1 then 0 else yi+1),(xf,yf),lc++[(xi,yi)],com+1)
          c4 = caminho lab ((xi  ,if yi == 0 then lar-1 else yi-1),(xf,yf),lc++[(xi,yi)],com+1)




---------------------------------------------------------------------------------------------------------------------

-- 2º Algoritmo : 
-- EXPERIMENTAR TORCAR A ORDEM PARA U,R,D,L
caminhof :: Maze -> Coords -> Coords -> (Int,[[Int]])
caminhof lab (xi,yi) (x,y) = caminho  (mudalab lab) ((xi,yi),(x,y))
--                           caminho (trocapeca (mudalab lab) (x,y) 0) ((xi,yi),(x,y))


caminho :: [[Int]] -> (Coords,Coords) -> (Int,[[Int]])
--                   partida,chegada-> (comprimento, lab)
caminho lab ((xi,yi),(xf,yf))
    | (xi,yi) == (xf,yf)       = (0 ,trocapeca lab (xi,yi) 0)
    -- | elem (xi,yi) lc          =- (n+1001 ,lab4) -- lc é a lista das coords perc
    | lab !! xf !! yf >= inf = (inf,lab) -- 10000 é o número correspondente a uma WAll
    | lab !! xi !! yi >= inf = (inf,lab)
    | otherwise = let
          lar = length (head lab)
          
          pecaU = lab !! (xi-1) !! yi
          pecaD = lab !! (xi+1) !! yi
          pecaL = lab !!  xi    !! (if yi == 0 then lar-1 else yi-1)
          pecaR = lab !!  xi    !! (if yi == lar-1 then 0 else yi+1)

          lab0 = trocapeca lab (xi,yi) inf

          p1@(c1,lab1) = if pecaU == -1
                         then caminho lab0 ((xi-1,yi) ,(xf,yf))
                         else (pecaU, lab0)
          p2@(c2,lab2) = if pecaD == -1
                         then caminho lab1 ((xi+1,yi) ,(xf,yf))
                         else (pecaD, lab1)
          p3@(c3,lab3) = if pecaL == -1
                         then caminho lab2 ((xi,if yi==0 then lar-1 else yi-1) ,(xf,yf))
                         else (pecaL, lab2)
          p4@(c4,lab4) = if pecaR == -1
                         then caminho lab3 ((xi,if yi==lar-1 then 0 else yi+1) ,(xf,yf))
                         else (pecaR, lab3)

          (cf,_) = head (sortOn fst [p1,p2,p3,p4])

          lab5 = trocapeca lab4 (xi,yi) (cf+1)

    in  (cf+1,lab5)
