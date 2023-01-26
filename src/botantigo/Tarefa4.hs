module Tarefa4 where
--import Tarefa1 
import Types
import Tarefa5
import Tarefa2
import Tarefa6
--step consiste num número de quantas iterações o jogo já passou.

defaultDelayTime = 250--250
--FIXME MUDAR MANUALMENTE NA TAREFA 2


--execução de jogadas por parte de todos os jogadores em jogo
--para cada jogador fazer uma jogada, tenho que fazer isto x vezes 

-- | 
passTime :: Int -> State -> State
--passTime 0 e  = e
passTime x e@(State lab l nivel) = (repetir l x e) 


-- Para cara jogador preciso: UMA ORIENTAÇAO, O RESPETIVO ID
--     Para fazer fazer uma jogada: play (Move id ori) e
jogar1 :: Int -> Player -> State -> State

jogar1 z pl@(Pacman(PacState (idj,(x,y),v,ori,p,vidas) t boca es)) e = if es == Dying then e else play movi e
        where orip = ori -- FIXME 
              movi = aux (bot idj e)
              aux (Just p) = p
              aux (Nothing)= (Move idj ori)
                       
--jogar1 z pl@(Pacman(PacState (idj,(x,y),v,ori,p,vidas) t boca es)) e = if es == Dying then e else play (Move idj orip) e
--        where orip = ori -- FIXME 



jogar1 z p@(Ghost(GhoState(id,cordg,vm,orig,pg,vidasg) Alive)) e = play (chaseMode e id) e
jogar1 z p@(Ghost(GhoState(id,cordg,vm,orig,pg,vidasg) Dead)) e
     -- = play (scatterMode e id) e
     | even z = play (scatterMode e id) e
     | otherwise = e



-- TENHO QUE FAZER ISTO PARA OS OUTROS JOGADORES

-- Aqui vamos repetir a função jogar1 para todos os Players 
repetir :: [Player]-> Int  -> State -> State
repetir [] x e = e 
repetir (h:t) x e = repetir t x (jogar1 x h e)




