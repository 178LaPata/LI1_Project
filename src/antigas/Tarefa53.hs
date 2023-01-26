module Tarefa53 where
import Types
import Tarefa1
import Tarefa2
import Tarefa3
import Data.List



--chaseMode :: State -> Int -> Play
--chaseMode (State lab lp@(jogador:fs) l) x =

--scatterMode :: State -> Int -> Play

--ghostPlay :: State -> [Play]

{-
 U  (-1,0 )
 D  (1 ,0 )
 R  (0 ,1 )
 L  (0 ,-1)
-}

inf = 10000 -- # número convencionado de INFINITO

--troca uma peça
trocapeca :: [[Int]] -> (Int,Int) -> Int -> [[Int]]
trocapeca m (x,y) v = a++corredor:t2
    where (a,(h:t2)) = splitAt x m
          (q,(_:t))  = splitAt y h
          corredor = q++v:t

mudalab :: Maze -> [[Int]]
mudalab l      = map mudacorredor l

mudacorredor :: Corridor -> [Int]
mudacorredor c = map mudapeca c

mudapeca :: Piece -> Int
mudapeca Wall          = inf
mudapeca Empty         = -1
mudapeca (Food Little) = -1
mudapeca (Food Big)    = -1


--caminho g ((4,4),(4,10),[],0)
--
--g1= generateMaze 15 15 1

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
          pecaU = lab !! (xi-1) !! yi
          pecaD = lab !! (xi+1) !! yi
          pecaL = lab !!  xi    !! (if yi == 0 then lar-1 else yi-1)
          pecaR = lab !!  xi    !! (if yi == lar-1 then 0 else yi+1)

          lab0 = trocapeca lab (xi,yi) inf

          lar = length (head lab)
          n = (length lab) * lar -- caminho maior possivel é área do lab
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



-- calcula caminho com uma matriz de caminhos mais curtos


{-
--BASTA CALCULAR O PRIMEIRO PASSO
cmc :: (Int,Int) -> [[Int]] -> Orientation
cmc (x,y) m = (snd $ head lf)
    where
        pecaU = (m !! (x-1) !! y,U)
        pecaD = (m !! (x+1) !! y,D)
        pecaL = (m !!  x    !! (if y == 0 then lar-1 else y-1),L)
        pecaR = (m !!  x    !! (if y == lar-1 then 0 else y+1),R)

        lar = length $ head m
        lf = sortOn fst  $ [pecaU,pecaD,pecaR,pecaL]
-}


cmcfinal :: Coords -> Coords -> Maze -> Maybe [Orientation]
cmcfinal ci cf lab = if (length  l) == 10000 then Nothing else Just (cmc c ci labint) 
    where 
        l = cmc c ci labint

        (c,labint) = caminhof lab ci cf 
        

cmc ::  Int -> (Int,Int) -> [[Int]] -> [Orientation]
cmc 0 (_,_) _ = []
cmc c (x,y) m = origo : cmc (c-1) (x+x2, yfinal origo lar y )  m
    where
        pecaU = m !! (x-1) !! y
        pecaD = m !! (x+1) !! y
        pecaL = m !!  x    !! (if y == 0 then lar-1 else y-1)
        pecaR = m !!  x    !! (if y == lar-1 then 0 else y+1)
        lar = length $ head m
        origo = snd  $ head lf
        (x2,y2) = head [ vetor | (ori,vetor) <- [(U,(-1,0)),(D,(1,0)), (R,(0,1)), (L,(0,-1))] , ori == origo ]

       -- (x2,y2) = snd $ head $ filter ((==)(snd $ head lf)) [(U,(-1,0)),(D,(1,0)), (R,(0,1)), (L,(0,-1))]
        lf = sortOn fst $ zip [pecaU,pecaD,pecaR,pecaL] [U,D,R,L]

        yfinal :: Orientation -> Int -> Int -> Int
        ---       origo         lar     y     y2 ->   yf
        yfinal R lar y  = if y == lar-1 then 0 else y+1
        yfinal L lar y  = if y == 0 then lar-1 else y-1
        yfinal _ _   y  = y







































{-caminho :: [[Int]] -> (Coords,Coords,[Coords],Int) -> (Int,[Coords],[[Int]])
--              partida,chegada,percorrido,comprimento -> (comprimento, perc, lab)
caminho lab ((xi,yi),(xf,yf),lc,com)
    | (xi,yi) == (xf,yf)       = (0  ,[(xi,yi)],lab4)
    | elem (xi,yi) lc          = (n+1000  ,lc,lab4) -- lc é a lista das coords perc
    | lab !! xf !! yf >= n = (n+2000  ,lc,lab) -- 10000 é o número correspondente a uma WAll
    | lab !! xi !! yi >= n = (n+3000 ,lc,lab)
    | otherwise = (cf+1,lf,trocapeca lab4 (xi,yi) (cf+1))

    where pecaU = lab !! (xi-1) !! (yi  )
          pecaD = lab !! (xi+1) !! (yi  )
          pecaL = lab !! (xi  ) !! (if yi == 0 then lar-1 else yi-1)
          pecaR = lab !! (xi  ) !! (if yi == lar-1 then 0 else yi+1)

          lar = length (head lab)
          n = (length lab) * lar -- caminho maior possivel é área do lab

          p1@(c1,lc1,lab1) = if pecaU == -100
                          then caminho lab ((xi-1,yi) ,(xf,yf),lc++[(xi,yi)],com+1)
                          else (pecaU,lc++[(1000,1000)], lab)

          p2@(c2,lc2,lab2) = if pecaD == -100
                          then caminho lab1 ((xi+1,yi) ,(xf,yf),lc++[(xi,yi)],com+1)
                          else (pecaU,lc++[(1000,1000)], lab1)

          p3@(c3,lc3,lab3) = if pecaL == -100
                          then caminho lab2 ((xi,if yi==0 then lar-1 else yi-1) ,(xf,yf),lc++[(xi,yi)],com+1)
                          else (pecaU,lc++[(1000,1000)], lab2)

          p4@(c4,lc4,lab4) = if pecaR == -100
                          then caminho lab3 ((xi,if yi==lar-1 then 0 else yi+1) ,(xf,yf),lc++[(xi,yi)],com+1)
                          else (pecaU,lc++[(1000,1000)], lab3)


          (cf,lf,_) = head (sortOn (\(a,_,_) -> a) [p1,p2,p3,p4])
        -}
