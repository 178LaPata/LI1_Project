module Tarefa6 where
import Data.List 
import Types
import FileUtils
import Tarefa5
--import Ft


{-
O QUE É PRECISO FAZER?

Ideia: 
    o pacman tem tres modos: normal, fuga e caça. 
    no modo normal tem a restrição de nao poder comer comida grande -> consiste apenas em andar tranquilo a comer
    no modo de fuga tem que fugir dos fantasmas
    o modo de caça é ativado quando o pacman fica encurralado entre dois fantasmas -> nao tem restrições, apenas tem que comer comida grande 

    ri a correr buscar uma comida, e tenta ir matar os ghost 1,5 segundos antes de acabar o temp vai a procura da proxima comida 
    ir a uma comida
    ir a um fantasma
    
-}



-- | Calcula uma jogada do bot. Utiliza um método de ir procurar uma comida grande, e depois ir comer fantasmas

bot :: Int -> State -> Maybe Play
bot x e@(State lab ( (Pacman (PacState (i,c,v,o,p,vv) time boca es)) :fs) l) 
    | es == Mega   = if null k 
                     then Just (Move i  (cmcfl c (fx,fy) lab)) -- FIXME ATIVAR FUGIR FANTASMA
                     else if time < 1.5
                          then Just (Move i (cmcf c (cx,cy) lab))
                          else Just (Move i (cmcf c (fx,fy) lab)) -- se esté em mega vai comer fantasmas( o de id 1).


    | es == Normal = if null k 
                     then Just (Move i  (cmcfl c (fx,fy) lab)) 
                     else Just (Move i (cmcf c (cx,cy) lab))-- se está em modo normal calcula o melhor caminho para uma food big
    | es == Dying  = Nothing 
    where (cx,cy) = if null k then c else head k-- FIXME
          (px,py) = c
          k = [ (a,b) | a <- [0..com-1], b <- [0..lar-1], lab !! a !! b  == Food Big ] -- filtra todas as posiçoes com comida grande
          lar = length $ head lab 
          com = length lab 
          --(Ghost (GhoState (_,(fx,fy),_,_,_,_) _)) = head [g| g@(Ghost (GhoState (idj,(fx,fy),_,_,_,_) _))  <- fs,idj == 1 ]
           -- pega no ghost de id 1.
          lismp (px,py) = [ (dist (px,py) (fx,fy), (fx,fy)) | (Ghost (GhoState (idj,(fx,fy),_,_,_,_) modo)) <- fs ]-- deolve uma lista de pares com (distacnai do pacman ao fantasma, coordenadas do fantasma)
          -- JÀ CALCULO O MELHOR FANTASMA PARA COMER  
          (d,(fx,fy)) = head $ sortOn fst ( lismp (px,py) ) -- ordena
          --                   sort
                   


-- | Calcula a distância entre dois pontos
dist (x1 , y1) (x2 , y2) = sqrt (x'*x' + y'*y')
    where
      x' = fromIntegral x1 - fromIntegral x2
      y' = fromIntegral y1 - fromIntegral y2
