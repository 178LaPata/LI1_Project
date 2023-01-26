{- |

= INTRODUÇÃO
Tentatmos calcular o efeito da passagem de um instante de tempo num
estado do jogo.


= OBJETIVO
O objetivo é implementar uma unidade temporal
adicional que passou desde a última atualização e o estado do jogo, que contém a informação
necessária para processar uma jogada.
Fizemos umas funções para receber um estado, poe o pacman como primeiro elmento com id 0 e os fantasmas com outros ids

= CONCLUSÃO
Fomos bem sucedidos em implementar a passagem de tempo num estado de jogo, porém só conseguimos implementar a passagem de 
-}

module Tarefa4 where
import Types
import Tarefa5
import Tarefa2
import Tarefa6


defaultDelayTime = 250


-- | Dou update depois de cada iteração
passTime :: Int -> State -> State
passTime 0 e  = e
passTime x e@(State lab l nivel) = (repetir l x e) 


-- | Calcula a jogada de um player 
jogar1 :: Int -> Player -> State -> State

jogar1 z pl@(Pacman(PacState (idj,(x,y),v,ori,p,vidas) t boca es)) e = 
        if es == Dying then e else play movi e
    where 
        movi = aux (bot idj e)
        aux (Just p)  = p
        aux (Nothing) = (Move idj ori)                      
{-
jogar1 z pl@(Pacman(PacState (idj,(x,y),v,ori,p,vidas) t boca es)) e = 
            if es == Dying 
            then e 
            else play (Move idj orip) e
        where orip = ori 
-}

jogar1 z p@(Ghost(GhoState(id,cordg,vm,orig,pg,vidasg) Alive)) e = play (chaseMode e id) e
jogar1 z p@(Ghost(GhoState(id,cordg,vm,orig,pg,vidasg) Dead)) e
     -- = play (scatterMode e id) e
     | even z    = play (scatterMode e id) e
     | otherwise = e


-- | Aqui vamos repetir a função jogar1 para todos os Players 
repetir :: [Player]-> Int  -> State -> State
repetir [] x e = e 
repetir (h:t) x e = repetir t x (jogar1 x h e)


