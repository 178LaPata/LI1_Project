module Ft where
import Tarefa2
import FileUtils 
import Tarefa1
import Tarefa5
import Tarefa4
import Types


g  = generateMaze 20 20 1
--g  = generateMaze 20 50 1
g1 = generateMaze 15 15 1
g3 = generateMaze  5  5 3
g4 = generateMaze 100 100 1

-- é uma estado 
ef = (loadMaze "maps/1.txt")


-- | Põe os pontos nos is, pega no ef do stor e põe os id "corretos"
popni :: State -> State
popni (State lab lp n) = (State lab ( midFinal lp ) n)


-- | Filtra o pacman dos fantasmas
--                        pacman,  fantasmas
filtrapf :: [Player] -> ([Player], [Player])
filtrapf [] = ([], []) 
filtrapf (pl@(Pacman(PacState (idj,(x,y),v,ori,p,vidas) tempo boca es)):t) = (pl:e,d)
    where (e,d) = filtrapf t
filtrapf (p@(Ghost(GhoState(id,cordg,vm,orig,pg,vidasg) modo)):t) = (e,p:d)
    where (e,d) = filtrapf t 


-- | Muda o id do pacman para 0 
mudaidp :: Player -> Player
mudaidp (Pacman(PacState (idj,(x,y),v,o,p,vi) t b es)) = Pacman(PacState (0,(x,y),v,o,p,vi) t b es) 


-- | Muda os ids dos fantasmas 
mudaidf :: Int -> Player -> Player 
mudaidf x (Ghost(GhoState(id,cd,vm,o,pg,vi) m)) =Ghost(GhoState(x,cd,vm,o,pg,vi) m) 


-- | Muda id para todos os fantasmas
mtf :: [Player]-> Int -> [Player] 
mtf [] c = [] 
mtf (h:t) c =  mudaidf c h : mtf t (c+1)


midFinal :: [Player] -> [Player]
midFinal [] = [] 
midFinal l  = novop : novofs
    where (p,fs) = filtrapf l
          novop  = mudaidp (head p)
          novofs = mtf fs 1


-- | Exemplo de estado
e = State g jogador 9


-- | Exemplo de lista de players
jogador :: [Player]
jogador = [Pacman (PacState (0,(7,10),1,R,0,1) 0 Open Normal),
            Ghost (GhoState (1,(3,8 ),1,R,0,3)       Alive),  
            Ghost (GhoState (2,(2,1),1,R,0,3)       Alive)]


-- | Calcula o estado dada uma lista de orientações
joga :: [Orientation] -> State -> State 
joga l e = foldl (\a x -> play (Move 0 x) a) e l 









