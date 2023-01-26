
{- |

= INTRODUÇÃO

Nesta tarefa, tentamos calcular o caminho mais curto entre duas posições num labirinto para 
devolver aos fantasmas a orientação mais acertiva. Podemos considerar que, na nossa opinião, 
esta foi a tarefa mais dificil, sendo também a que deu mais trabalho, devido ao cálculo do 
algoritimo de procura do melhor caminho.

= OBJETIVO

O nosso principal objetivo foi conseguir encontrar um algoritmo que permitisse encontrar 
o melhor caminho até às coordenadas dos fantasmas. 


===Primeira tentativa

Começamos por tentar calcular o caminho 
mais curto experimentando todos os caminhos possíveis (1º algoritmo na Algoritmo.hs).
Contudo, chegamos à conclusão de que:

* Este metódo só era eficiente para labirintos até 10x10, 
mas para dimensões maiores demorava demasiado tempo para calcular o caminho mais curto.


===Segunda tentativa

Para otimizar, experimentamos escrever numa matriz de números o 
comprimento do caminho percorrido até ao momento no respetivo arco do percurso.
Nesta variante calculamos o caminho recursivamente numa direçao, depois devolvemos a 
matriz de comprimentos e davamos-la para as outras direções (2º algoritmo no Algoritmo.hs). 
Conluimos que, ao aplicar esta forma recursiva mais linearmente (para as 4 direções) 
constata-se que:

* Que funciona muito rápido para labirintos de grandes dimensões (200x200),
* Dá prioridade a alguma direções,por vezes não calcula o caminho mais curto.


=== Terceira variante: 

Conseguimos fazer um algoritmo que cumpre todos os requesitos. 
Este começa por marcar o destino como 0 depois marcamos os vizinhos do 0 com 1 
(de comprimento) se estes não forem paredes e se não tiverem já um valor inferior, 
depois marcamos os vizinhos de cada 1 com 2 e assim sucessivamente. No final de ambos 
os algoritmos, utilizamos uma função de descompactação que dado uma matriz com os 
comprimentos calcula uma lista de orientaçoes até chegar ao destino (função cmcfinal2). 
Para fins mais práticos usamos uma outra que nos devolve apenas a primeira orientação
(necessária para o movimento). 


Para fugir, utilizamos um algoritmo rudimentar de fuga, 
sendo este o seguerido no enunciado.

= CONCLUSÃO

Conseguimos calcular o caminho mais curto de forma bastante eficiente e rápida.

-}

module Tarefa5 where
import Types
import Data.List


inf = 10000 -- Número convencionado de infinito


-- | Estimativa de comprimento máximo

cm m = comp+alt 
    where 
        comp = length ( head m ) -1
        alt  = length m -1


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
                         y <- [0..lar-1] , -- Todas a combinações 
                         (v,(vx,vy)) <- vizinho m (x,y), -- Calcula as posiçoes R U D L 
                         (m!!x!!y)==n, -- O número no local tem que ser n 
                         v >= n+1,  -- Não posso mudar vizinhos que já mudei 
                         v < inf]   -- Não pode ser parede 
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


-- | Substitui num Labirinto determinadao valor 
sub :: [(Int,Int,Int)] -> [[Int]] -> [[Int]]
sub [] m = m 
sub ((x,y,v):t) m = sub t (trocapeca m (x,y) v)  


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
        lar   = length $ head m
        lf    = sortOn fst [pecaU,pecaD,pecaR,pecaL]


-- | Calcula o primeiro passo para o caminho mais longo
cmcfl :: Coords -> Coords -> Orientation -> Maze -> Orientation
cmcfl ci cf ori lab | ci == cf  = Null
                    | otherwise = cmcl ori ci labint
    where labint = caminhof2 lab ci cf


-- | Auxiliar cmcfl
cmcl :: Orientation -> (Int,Int) -> [[Int]] -> Orientation
cmcl ori (x,y) m = (snd $ last lf)
    where
        pecaU = (m !! (x-1) !! y,U)
        pecaD = (m !! (x+1) !! y,D)
        pecaL = (m !!  x    !! yfL,L)
        pecaR = (m !!  x    !! yfR,R)
        yfL   = _mov y (-1) lar
        yfR   = _mov y ( 1) lar
        lar = length $ head m------ priviligia a direçao que já está virado
        lf =  sortOn (\(d,o) -> if o == ori then d +1 else d ) 
              [(d,o) | (d,o)<- [pecaU,pecaD,pecaR,pecaL], d < (cm m )] 
        

-- | Calcula uma função dependendo do estado de um fantasma
chaseMode :: State -> Int -> Play
chaseMode e@(State lab (jogador:fs) nivel) x = (Move x ori)

    where Pacman(PacState (_,cp,_,_,_,_) _ _ _) = jogador
          Ghost (GhoState (_,cg,_,_,_,_) _) = ff2 x fs
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


-- | Calcula uma jogada para um fantasma em modo Dead
scatterMode :: State -> Int -> Play
scatterMode (State lab (jogador:fs) l) xid = (Move xid ori)
    where (Ghost (GhoState (idj,(x,y),vgg,or,p,vg) m)) = ff2 xid fs
          (u2,v2) = snd $ pd or
          (u3,v3) = pf or
          lar = length $ head lab 
          peca_direita = lab !! (x+u2) !! (_mov y v2 lar)
          peca_frente  = lab !! (x+u3) !! (_mov y v3 lar)       
          ori = if or  == Null 
                then R -- vira para a direita caso seja nula 
                else    
                    case (peca_frente, peca_direita) of
                    (Wall, _  ) -> fst $ pd or
                    ( _  , _  ) -> or 
      

-- | Calcula uma lista de orientações de ditam o caminho mais curto
cmcfinal2 :: Coords -> Coords -> Maze -> Maybe [Orientation]
cmcfinal2 ci@(x,y) cf lab = if (length  l) == 10000 then Nothing else Just (cmc2 c ci labint) 
    where 
        l      = cmc2 c ci labint
        labint = caminhof2 lab ci cf 
        c      = labint !! x !! y 

-- | Auxiliar cmcfinal2
cmc2 ::  Int -> (Int,Int) -> [[Int]] -> [Orientation]
cmc2 0 (_,_) _ = []
cmc2 c (x,y) m = origo : cmc2 (c-1) (x+x2, yfinal origo lar y )  m
    where
        pecaU   = m !! (x-1) !! y
        pecaD   = m !! (x+1) !! y
        pecaL   = m !!  x    !! (if y == 0 then lar-1 else y-1)
        pecaR   = m !!  x    !! (if y == lar-1 then 0 else y+1)
        lar     = length $ head m
        origo   = snd  $ head lf
        (x2,y2) = head [ vetor | (ori,vetor) <- [(U,(-1,0)),(D,(1,0)), (R,(0,1)), (L,(0,-1))] , ori == origo ]
        lf = sortOn fst $ zip [pecaU,pecaD,pecaR,pecaL] [U,D,R,L]

        yfinal :: Orientation -> Int -> Int -> Int
        yfinal R lar y  = if y == lar-1 then 0 else y+1
        yfinal L lar y  = if y == 0 then lar-1 else y-1
        yfinal _ _   y  = y
