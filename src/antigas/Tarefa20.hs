module Tarefa2 where 
import System.Random
import Tarefa1
import Types
--import Tarefa4
{-
 U  (-1,0 )
 D  (1 ,0 )
 R  (0 ,1 )
 L  (0 ,-1)
-}
 
-- | Calcula um estado depois de uma jogada  
play :: Play -> State -> State --jogador é o pacman PRIMEIRO ELEMENTO DA LISTA
play (Move id ori) (State lab (jogadore:fs) l) 
    | id == 0 = State (substitui pst lab) (vfantasmas (direcao ori pst lab) fs lab) l
    | id /= 0 = State lab (jogadore:k:b) l
    where (a,b) = ff id fs 
          k = mfantas lab ori a
          pst = temM jogadore



-- | Calcula a jogada de um fantasma
--    id      l           (==id,     /=id)
-- move os fantasmas
mfantas :: Maze -> Orientation -> Player -> Player
mfantas lab ori (Ghost (GhoState (idj,(x,y),vl,orig,pg,v) modo))--lista com 1 player filtrado
    | ori /= orig = Ghost (GhoState (idj,(x,y),vl,ori,pg,v) modo)
    | otherwise   = case ori of
        Null ->Ghost (GhoState (idj,(x,  y)                          ,vl,ori,pg,v) modo)
        U ->   Ghost (GhoState (idj,(x-1,y)                          ,vl,ori,pg,v) modo)
        D ->   Ghost (GhoState (idj,(x+1,y)                          ,vl,ori,pg,v) modo)
        R ->   Ghost (GhoState (idj,(x,yf ),vl,ori,pg,v) modo)
        L ->   Ghost (GhoState (idj,(x,yf ),vl,ori,pg,v) modo)
    where lar = length (head lab)
          yf = _mov y (snd ( pf ori) ) lar


-- | filtra fantasmas por id  ([ghost com id ], [resto dos fs])
ff :: Int -> [Player] -> (Player,[Player])
ff id l    = (head [ g | g@(Ghost (GhoState (idj,cordg,vl,orig,pg,vidasg) modo)) <- l, id == idj ] , k)
    where k =      [ g | g@(Ghost (GhoState (idj,cordg,vl,orig,pg,vidasg) modo)) <- l, id /= idj ]


-- | Subtrai tempo ao cronómetro to time Mega, colocando no respetivo estado
temM :: Player -> Player 
temM pac 
    | time <= 0 = Pacman (PacState (i,c,v,o,p,vv) 0 boca Normal)
    | otherwise = Pacman (PacState (i,c,v,o,p,vv) (time-constante_tempo) boca Mega)
    where         Pacman (PacState (i,c,v,o,p,vv) time boca es) = pac
                  constante_tempo = defaultDelayTime / 1000  
                  defaultDelayTime = 250 -- FIXME 






-- | Devolve um player dependendo da sua orientação
direcao :: Orientation -> Player -> Maze -> Player  -- se o jogador tiver virado na direçao do movimento entao mexo o jogador caso contrio mudo a direção
direcao ori pl@(Pacman (PacState (idj,cordp,v,oripl,p,vidas) time boca es)) lab 
    | ori == Null  = pl
    | ori == oripl = mexe ori pl lab
    | otherwise = Pacman (PacState (idj,cordp,v,ori,p,vidas) time boca es)


-- | Devolve um player depois de o mexer
mexe :: Orientation -> Player -> Maze -> Player
mexe U pl lab = ve_a_frente pl lab (-1,0 )
mexe D pl lab = ve_a_frente pl lab (1 ,0 )
mexe R pl lab = ve_a_frente pl lab (0 ,1 )
mexe L pl lab = ve_a_frente pl lab (0 ,-1)--vetor
-- x y , y corresponde ao numero do corredor
--       x é a altura

--TEM NO TYPES
{-
_mov y v2 lar
    | v2 == -1 = if y == 0 then lar-1 else y-1
    | v2 ==  1 = if y == lar-1 then 0 else y+1
    | v2 ==  0 = y
-}
-- | Devolve um Player calculando as suas novas coordenadas, pontos e modo
ve_a_frente :: Player -> Maze -> (Int,Int) -> Player-- ve o que jogador tem a frente, se não tiver parede então avança para a frente
ve_a_frente (Pacman(PacState (idj,(x,y),v,ori,p,vidas) t boca es)) lab (u,v2) = 
    case peca_local of
        Wall        -> Pacman (PacState (idj,(x  ,y ),v,ori,p,  vidas) t boca es)
        Food Little -> Pacman (PacState (idj,(x+u,yf),v,ori,p+1,vidas) t contraboca es)
        Food Big    -> Pacman (PacState (idj,(x+u,yf),v,ori,p+5,vidas) tmaximo contraboca Mega)
        Empty       -> Pacman (PacState (idj,(x+u,yf),v,ori,p  ,vidas) t contraboca es)

    where peca_local = lab !! (x+u) !! yf -- retira peça do labirinto
          yf = (_mov y v2 clab)
          clab = length $ head lab -- comprimento lab
          contraboca = contrario_boca boca
          tmaximo = 10 -- FIXME 




-- | Dado um player pacman e uma lista de players fantasmas, devolve uma lista de players com os seus estados calculados 
vfantasmas :: Player -> [Player] -> Maze -> [Player]
-- dou um pacman e uma lista de fantasmas        --sai pacman e os fs

vfantasmas pl@(Pacman (PacState (idj,cordp,v,oripl,p,vidas) time boca es)) fs lab = 
    if ncoli /= 0
    then if (es == Mega) 
         then (pl'   :[megafantasma f cordp lab | f <- fs] )
         else (pmorto:[casafantasma  f      lab | f <- fs] )
    else if (es == Mega)
         then (pl:(ptm fs))
         else (pl:(pfv fs))

    where pl'    = Pacman (PacState (idj,cordiniciais,v,oripl,p+10*ncoli,vidas) time boca Mega)
          pmorto = if (vidas-1) == 0 then Pacman (PacState (idj,cordiniciais,v,oripl,p         ,vidas-1) time boca Dying)
                   else                   Pacman (PacState (idj,cordiniciais,v,oripl,p         ,vidas-1) time boca Normal)
          cordiniciais = cordp --FIXME defenir coordenadas iniciais depois 
          ncoli = colisoes pl fs


-- | Poe um lista de fantasmas em modo Alive
pfv :: [Player] -> [Player]
pfv l = [  (Ghost (GhoState (id,c,0.5,o,pg,vi) Alive)) | (Ghost (GhoState (id,c,v,o,pg,vi) modo))  <- l ]
-- |Poe todos os fantasmas em modo Dead
ptm :: [Player] -> [Player] 
ptm l  = [  (Ghost (GhoState (id,c,0.5,o,pg,vi) Dead)) | (Ghost (GhoState (id,c,v,o,pg,vi) modo))  <- l ]

-- | Dado um player fantasma, as coordenadas do pacman em estado Mega e um labirinto calcula o estado do fantasma 
megafantasma :: Player -> (Int,Int) -> Maze -> Player
-- quando colide e o pacma esta em mega entao mudo o estado dos ghost
megafantasma (Ghost (GhoState (id,cordg,vl,orig,pg,vidasg) modo)) cordp lab
    | cordg == cordp = Ghost (GhoState (id,cordcasa,0.5,U,pg,vidasg)  Dead )
    | otherwise      = Ghost (GhoState (id,cordg   ,0.5,orig,pg,vidasg) Dead)
    where cordcasa = casaf lab
          vm = vl --FIXME defenir velocidade metade depois


-- | Devolve todos os fantasmas dentro da casa fantasma, dado uma lista de players fantasmas
casafantasma :: Player -> Maze -> Player -- quando o pacman morre, poe todos os fantasmaas em casa
casafantasma (Ghost (GhoState (id,cordg,vl,orig,pg,vidasg) modo)) lab = 
    Ghost (GhoState (id,cordcasa,vl,U,pg,vidasg) Alive)
    where cordcasa = casaf lab
          vi = vl --FIXME defenir inicial depois 


-- | Calcula o número de colisões de um player pacman com uma lista de players fantasmas
colisoes :: Player -> [Player] -> Int 
colisoes pl@(Pacman  (PacState (_,cordp,_,_,_,_) _ _ _)) fs = length (filter c fs)-- fs é a lista de fantasmas
    where c (Ghost (GhoState (_,cordg,_,_,_,_) _)) = cordg == cordp
-- | Devolve o contrário do estado da boca, aberto ou fechada
contrario_boca :: Mouth -> Mouth
contrario_boca Open = Closed
contrario_boca Closed = Open 

-- | Calcula o labirinto subtituindo por espaços vazios os síteos por onde o pacman passa 
substitui :: Player -> Maze -> Maze
substitui (Pacman (PacState (_,(x,y),_,_,_,_) _ _ _)) lab = replaceElemInMaze (x,y) Empty lab

-- | Calcula as coord do meio da casa dos fantasmas, aproximadamente
casaf :: Maze -> (Int,Int)
casaf lab = (div (length lab) 2,div (length (head lab)-1) 2 )


--casaf lab = (div (length (head lab)-1) 2 ,div (length lab) 2)




