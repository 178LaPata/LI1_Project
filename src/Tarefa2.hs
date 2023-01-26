{- | 

= INTRODUÇÃO

Para esta tarefa o objetivo é a implementação de realização de jogadas para jogadores do tipo Pacman e Ghost

= OBJETIVO

O objetivo desta tarefa é, dada uma descrição do estado do jogo e uma jogada de um dos
jogadores, determinar o efeito dessa jogada no estado do jogo.

= CONCLUSÃO

Conseguimos atualizar o efeito de uma jogada.

-}

module Tarefa2 where 
import System.Random
import Tarefa1
import Types


-- | Calcula um estado depois de uma jogada  
play :: Play -> State -> State 
play (Move id ori) (State lab (jogadore:fs) l) 
    | id == 0 = State (substitui pst lab) (vfantasmas (direcao ori pst lab) fs lab) l
    | id /= 0 = State lab (jogadore:k:b) l
    where (a,b) = ff id fs 
          k = mfantas lab ori a
          pst = temM jogadore



-- | Calcula a jogada de um fantasma
mfantas :: Maze -> Orientation -> Player -> Player
mfantas lab ori (Ghost (GhoState (idj,(x,y),vl,orig,pg,v) modo)) -- lista com 1 player filtrado
    | ori /= orig = Ghost (GhoState (idj,(x,y),vl,ori,pg,v) modo)
    | otherwise   = case ori of
        Null ->Ghost (GhoState (idj,(x,  y),vl,ori,pg,v) modo)
        U ->   Ghost (GhoState (idj,(x-1,y),vl,ori,pg,v) modo)
        D ->   Ghost (GhoState (idj,(x+1,y),vl,ori,pg,v) modo)
        R ->   Ghost (GhoState (idj,(x,yf ),vl,ori,pg,v) modo)
        L ->   Ghost (GhoState (idj,(x,yf ),vl,ori,pg,v) modo)
    where lar = length (head lab)
          yf = _mov y (snd ( pf ori) ) lar


-- | Filtra fantasmas por id  ([ghost com id ], [resto dos fs])
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
                  defaultDelayTime = 250 


-- | Devolve um player dependendo da sua orientação
direcao :: Orientation -> Player -> Maze -> Player  -- Se o jogador tiver virado na direção do movimento então mexo o jogador caso contrario mudo a direção
direcao ori pl@(Pacman (PacState (idj,cordp,v,oripl,p,vidas) time boca es)) lab 
    | ori == Null  = pl
    | ori == oripl = mexe ori pl lab
    | otherwise = Pacman (PacState (idj,cordp,v,ori,p,vidas) time boca es)


-- | Devolve um player depois de o mexer
mexe :: Orientation -> Player -> Maze -> Player
mexe U pl lab = ve_a_frente pl lab (-1,0 )
mexe D pl lab = ve_a_frente pl lab (1 ,0 )
mexe R pl lab = ve_a_frente pl lab (0 ,1 )
mexe L pl lab = ve_a_frente pl lab (0 ,-1) -- vetor
-- y -> número do corredor
-- x -> altura


-- | Devolve um Player calculando as suas novas coordenadas, pontos e modo
ve_a_frente :: Player -> Maze -> (Int,Int) -> Player -- Ve o que jogador tem a frente, se não tiver parede então avança para a frente
ve_a_frente (Pacman(PacState (idj,(x,y),v,ori,p,vidas) t boca es)) lab (u,v2) = 
    case peca_local of
        Wall        -> Pacman (PacState (idj,(x  ,y ),v,ori,p,  vidas) t boca es)
        Food Little -> Pacman (PacState (idj,(x+u,yf),v,ori,p+1,vidas) t contraboca es)
        Food Big    -> Pacman (PacState (idj,(x+u,yf),v,ori,p+5,vidas) tmaximo contraboca Mega)
        Empty       -> Pacman (PacState (idj,(x+u,yf),v,ori,p  ,vidas) t contraboca es)
    where peca_local = lab !! (x+u) !! yf -- Retira peça do labirinto
          yf = (_mov y v2 clab)
          clab = length $ head lab -- comprimento lab
          contraboca = contrario_boca boca
          tmaximo = 10 


-- | Dado um player pacman e uma lista de players fantasmas, devolve uma lista de players com os seus estados calculados 
vfantasmas :: Player -> [Player] -> Maze -> [Player]
vfantasmas pl@(Pacman (PacState (idj,cordp,v,oripl,p,vidas) time boca es)) fs lab
    | es == Mega && time == 10 = pl:(ptm fs)
    | not $ null lcv = pmorto: [casafantasma f lab | f <- fs] 
    | not $ null lcm = pl' :  ([casafantasma f lab | f <-lcm] ++ outros)
    | null lcm && null lcv = pl : fs
    | es == Normal && time <= 0  = pl:(ptv fs)
    where pl'    = Pacman (PacState (idj,cordiniciais,v,oripl,p+10*(length lcm),vidas) time boca Mega)
          pmorto = if (vidas-1) == 0 
                   then Pacman (PacState (idj,cordiniciais,v,oripl,p,vidas-1) 0 boca Dying)
                   else Pacman (PacState (idj,cordiniciais,v,oripl,p,vidas-1) 0 boca Normal)
          cordiniciais      = cordp 
          (lcv,lcm,outros)  = colisoes pl fs


-- | Poe todos os fantasmas em modo Alive
ptv :: [Player] -> [Player]
ptv l =  [  (Ghost (GhoState (id,c,1,o,pg,vi) Alive)) | (Ghost (GhoState (id,c,v,o,pg,vi) m))  <- l ]


-- | Poe todos os fantasmas em modo Dead
ptm :: [Player] -> [Player] 
ptm l  = [  (Ghost (GhoState (id,c,0.5,o,pg,vi) Dead)) | (Ghost (GhoState (id,c,v,o,pg,vi) m))  <- l ]


-- | Devolve todos os fantasmas dentro da casa fantasma, dado uma lista de players fantasmas
casafantasma :: Player -> Maze -> Player -- Quando o pacman morre, põe todos os fantasmas em casa
casafantasma (Ghost (GhoState (id,cordg,vl,orig,pg,vidasg) modo)) lab = 
    Ghost (GhoState (id,cordcasa,1,U,pg,vidasg) Alive)
    where cordcasa = casaf lab
          vi = vl


-- | Calcula o número de colisões de um player pacman com uma lista de players fantasmas
colisoes :: Player -> [Player] -> ([Player],[Player],[Player])
colisoes pl@(Pacman  (PacState (_,cordp,_,_,_,_) _ _ _)) fs = (ncv,ncm,outros)
    where ncv = [ g  | g@(Ghost (GhoState (i,c,vl,o,p,v) m)) <- fs, cordp == c , m == Alive] 
          ncm = [ g  | g@(Ghost (GhoState (i,c,vl,o,p,v) m)) <- fs, cordp == c , m == Dead ] 
          outros=[ g | g@(Ghost (GhoState (i,c,vl,o,p,v) m)) <- fs, cordp /= c] 


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
