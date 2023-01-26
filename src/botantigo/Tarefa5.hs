module Tarefa5 where
import Types
import Data.List






inf = 10000 -- # número convencionado de INFINITO

cm m = comp+alt  -- estimativa de comprimento máximo
    where 
        comp = length ( head m ) -1
        alt  = length m -1
--troca uma peça
-- | Troca um número numa posiçao específica de uma matriz
trocapeca :: [[Int]] -> (Int,Int) -> Int -> [[Int]]
trocapeca m (x,y) v = a++corredor:t2
    where (a,(h:t2)) = splitAt x m
          (q,(_:t))  = splitAt y h
          corredor = q++v:t

-- | Tranforma um labirinto de Pieces numa matriz com números inteiros
mudalab :: Maze -> [[Int]]
mudalab l      = map mudacorredor l
    where
        mudacorredor :: Corridor -> [Int]
        mudacorredor c = map mudapeca c

        mudapeca :: Piece -> Int
        mudapeca Wall          = inf
        mudapeca Empty         = cm2
        mudapeca (Food Little) = cm2
        mudapeca (Food Big)    = cm2
        cm2 = cm l

-- | Calcula uma matriz com a distancia até ao destino em cada posição 
caminhof2 :: Maze -> Coords -> Coords -> [[Int]]
caminhof2 lab (xi,yi) (xf,yf) = caminhoaux (trocapeca (mudalab lab) (xf,yf) 0) (xi,yi) (xf,yf)


-- | Auxiliar da caimnhof2
caminhoaux :: [[Int]] -> Coords -> Coords -> [[Int]]
caminhoaux m (xi,yi) (xf,yf) = foldl cstep  m [0..cm2-1]
    where cstep ma n = sub (f1 ma n) ma 
          cm2 = cm m  

-- | Calcula os vizinhos de n filtrados, por substituir
f1 m n = [ (vx,vy,n+1) | x <- [0..alt-1] ,
                         y <- [0..lar-1] ,-- todas a combinaçoes 
                         (v,(vx,vy)) <- vizinho m (x,y), -- calcula as posiçoes R U D L 
                         (m!!x!!y)==n, -- o número no local tem que ser n 
                         v >= n+1,  --  nao posso mudar vizzinhos que já mudei 
                         v < inf]   -- não pode ser parede 

    where lar = length ( head m )
          alt  = length m 
 
-- | Calcula um par com os vizinhos e as suas coordenadas
vizinho :: [[Int]] -> (Int,Int) ->[ (Int, (Int,Int)) ]
vizinho lab (xi,yi) = [pecaU, pecaD, pecaR, pecaL]
    where pecaU = (lab !! (xi-1) !! yi, (xi-1,yi))
          pecaD = (lab !! (xi+1) !! yi, (xi+1,yi))
          pecaL = (lab !!  xi    !! yfL,(xi,yfL ))
          pecaR = (lab !!  xi    !! yfR,(xi,yfR ))
          lar   = length ( head lab )
          yfL   = _mov yi (-1) lar 
          yfR   = _mov yi ( 1) lar


--        x   y   v
-- | Substitui num Labirinto determinadao valor 
sub :: [(Int,Int,Int)] -> [[Int]] -> [[Int]]
sub [] m = m 
sub ((x,y,v):t) m = sub t (trocapeca m (x,y) v)  



-- calcula caminho com uma matriz de caminhos mais curtos

--BASTA CALCULAR O PRIMEIRO PASSO
-- | Calcula o primeiro passo para o caminho mais curto
cmcf :: Coords -> Coords -> Maze -> Orientation
cmcf ci cf lab | ci == cf  = Null
               | otherwise = cmc ci labint
    where labint = caminhof2 lab ci cf 
-- | Auxuliar da cmcf
cmc :: (Int,Int) -> [[Int]] -> Orientation
cmc (x,y) m = (snd $ head lf)
    where
        pecaU = (m !! (x-1) !! y,U)
        pecaD = (m !! (x+1) !! y,D)
        pecaL = (m !!  x    !! yfL,L)
        pecaR = (m !!  x    !! yfR,R)
        yfL   = _mov y (-1) lar
        yfR   = _mov y ( 1) lar
        lar = length $ head m
        lf = sortOn fst [pecaU,pecaD,pecaR,pecaL]

-------------------------------
--CAMINHO MAIS LONGOO ! METODO DE FUGA 
--BASTA CALCULAR O PRIMEIRO PASSO
-- | Calcula o primeiro passo para o caminho mais longo
cmcfl :: Coords -> Coords -> Maze -> Orientation
cmcfl ci cf lab | ci == cf  = Null
                | otherwise = cmcl ci labint
    where labint = caminhof2 lab ci cf

cmcl :: (Int,Int) -> [[Int]] -> Orientation
cmcl (x,y) m = (snd $ head lf)
    where
        pecaU = (m !! (x-1) !! y,U)
        pecaD = (m !! (x+1) !! y,D)
        pecaL = (m !!  x    !! yfL,L)
        pecaR = (m !!  x    !! yfR,R)
        yfL   = _mov y (-1) lar
        yfR   = _mov y ( 1) lar
        lar = length $ head m
        lf = reverse $ sortOn fst [pecaU,pecaD,pecaR,pecaL]
---

-- | Calcula uma função dependendo do estado de um fantasma
chaseMode :: State -> Int -> Play
chaseMode e@(State lab (jogador:fs) nivel) x = (Move x ori)-- x/= 0

    where Pacman(PacState (_,cp,_,_,_,_) _ _ _) = jogador
          Ghost (GhoState (_,cg,_,_,_,_) _) = ff2 x fs
          --Ghost (GhoState (idj,cg,vgg,or,p,_) m) = jogador !! x
          --FIXME
          ori = cmcf cg cp lab





-- | Filtra fantasma por id
ff2 :: Int -> [Player] -> Player
ff2 id l    = head [ g | g@(Ghost (GhoState (idj,cg,v,o,p,vi) m)) <- l, id == idj ]





-- | Calcula orientacao,vetor rodados para a direita
pd :: Orientation -> (Orientation,(Int,Int))
pd R = (D,(1 , 0))
pd D = (L,(0 ,-1))
pd L = (U,(-1, 0))
pd U = (R,(0 , 1))

-- vetor de mexer para frente 
--tem na TYPEa
{-
pf :: Orientation -> (Int,Int)
pf U = (-1,0 )
pf D = (1 ,0 )
pf R = (0 ,1 )
pf L = (0 ,-1)
-}

-- | Calcula uma jogada para um fantasma em modo Dead
scatterMode :: State -> Int -> Play
scatterMode (State lab (jogador:fs) l) xid = (Move xid ori)

    where (Ghost (GhoState (idj,(x,y),vgg,or,p,vg) m)) = ff2 xid fs
          (u2,v2) = snd $ pd or
          (u3,v3) = pf or
          lar = length $ head lab 
          peca_direita = lab !! (x+u2) !! (_mov y v2 lar)
          peca_frente  = lab !! (x+u3) !! (_mov y v3 lar)
          
          ori = if or == Null then R -- vira para a direita caso seja nula 
                else    
                    case (peca_frente, peca_direita) of
                    (Wall, _  ) -> fst $ pd or
                    ( _  , _  ) -> or 
                    --( _  ,Wall) -> or
            






-----------------------------------------------------------------------------------------------
-- DEVOLVE UMA LISTA DE ORIENTACOES
-- | Calcula uma lista de orientações de ditam o caminho mais curto
cmcfinal2 :: Coords -> Coords -> Maze -> Maybe [Orientation]
cmcfinal2 ci@(x,y) cf lab = if (length  l) == 10000 then Nothing else Just (cmc2 c ci labint) 
    where 
        l = cmc2 c ci labint

        labint = caminhof2 lab ci cf 
        c = labint !! x !! y 

cmc2 ::  Int -> (Int,Int) -> [[Int]] -> [Orientation]
cmc2 0 (_,_) _ = []
cmc2 c (x,y) m = origo : cmc2 (c-1) (x+x2, yfinal origo lar y )  m
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


