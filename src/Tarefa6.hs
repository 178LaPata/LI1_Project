
{- | 

= INTRODUÇÃO

Esta tarefa consistiu na criação de um bot, isto é, de um "jogador" capaz de jogar sem 
qualquer input humano. 

= OBJETIVO

Nesta tarefa, o nosso principal objetivo foi desenvolver um bot que conseguisse fazer 
as mesmas jogadas que um humano consegue fazer, tentando ser o mais idêntico possível.


Para alcançar esse objetivo, tivemos que passar por várias etapas. Numa primeira etapa 
decidimos que a melhor estratégias seriam fugir, comer uma  Big Food ou comer um fantasma.
Calculando a distância a um a comida grande e a um fantasma e sabendo o tempo em Mega 
restante consegue-se decidir qual é a melhor opção a o pacman proceder.


Para fugir utilizamos o algorítmo que calcula uma matriz com os comprimentos. A ideia é 
que como consigo calcular o caminho mais curto para um ponto (seguindo pelas casa com 
menor comprimento) consigo também calcular um caminho mais "longo"(seguindo pelas casa com 
caminho superior ao atual), ou seja, um caminho que se afasta do fantasma.

= CONCLUSÃO

Concluindo, tentamos fazer o melhor possível para que o nosso bot seja o mais parecido
com a maneira que um jogador humano jogaria gerindo o tempo e distância a comida e fantasmas.
Penso que fomos bem sucedidos e contudo, há sempre espaço para melhorias.

-}

module Tarefa6 where
import Data.List 
import Types
import FileUtils
import Tarefa5


-- | Calcula uma jogada do bot.Utiliza um método que analisa a distância a uma comida grande a um fantasma e o time mega
bot :: Int -> State -> Maybe Play
bot x e@(State lab ( (Pacman (PacState (i,c,v,o,p,vv) time boca es)) :fs) l) 
--     | es == Normal && null k && modo == Alive = fujo -- Fujo 
   
    | es == Normal && null k && dcp +3 < df = Just (Move i  (cmcf  c (cpx,cpy) lab))
   -- | es == Normal && null k && dcp <= df = Just (Move i  Null )
    | es == Normal && null k && dcp +3 >=  df = fujo  -- Fujo
    
    | es == Normal && dc <= df = Just (Move i (cmcf c (cx,cy)    lab))
    | es == Normal && dc >  df = fujo -- Fujo    
    | es == Mega   && modo == Dead  && time > 1.5  = Just (Move i  (cmcf c cf lab))
    | es == Mega   && modo == Dead  && time <= 1.5 = fujo 
    | es == Mega   && modo == Alive && dc <  df    = Just (Move i  (cmcf c (cx,cy) lab))
    | es == Mega   && modo == Alive && dc >= df = fujo -- Fujo 
    | es == Dying  = Nothing


    | otherwise = fujo --fujo
    
    -- | otherwise = Nothing 
    where
          (dc,(cx,cy)) = if null k then (1000,c) else head k -- para comida grande
          k   = sortOn fst [ (dist c (a,b), (a,b)) | a <- [0..alt-1], b <- [0..lar-1], 
                                                      lab !! a !! b  == Food Big ] -- Filtra todas as posiçoes com comida grande 
          -------- para comida pequena
          (dcp ,(cpx,cpy)) = if null cpe then (1000,c)   else head cpe
          cpe = sortOn fst [ (dist c (a,b), (a,b)) | a <- [0..alt-1], b <- [0..lar-1], 
                                                        lab !! a !! b  == Food Little, (a,b)/= c  ]
           ---------

          lismp  = [ (dist c cf, cf,modo) | (Ghost (GhoState (_,cf,_,_,_,_) modo)) <- fs ] 
          -- lista de pares com (distancia do pacman ao fantasma, coordenadas do fantasma,modo)
          (df,cf ,modo) = head $ sortOn (\(a,b,c) -> a) lismp -- Ordena
          
          fujo = Just (Move i  (cmcfl c cf o lab))
          
          lar = length $ head lab 
          alt = length lab 






-- | Calcula a distância entre dois pontos
dist (x1 , y1) (x2 , y2) = sqrt (x'*x' + y'*y')
    where
      x' = fromIntegral x1 - fromIntegral x2
      y' = fromIntegral y1 - fromIntegral y2
