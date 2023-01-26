module Main where

import Data.Time.Clock.POSIX
import Control.Monad.IO.Class
import UI.NCurses
import Types
import FileUtils
import Tarefa4
import Tarefa5
import Tarefa3
import Tarefa2
import Tarefa1
import Tarefa6
import Ft 

            

data Manager = Manager 
    {   
        state   :: State
    ,    pid    :: Int
    ,    step   :: Int
    ,    before :: Integer
    ,    delta  :: Integer
    ,    delay  :: Integer
    } 

eff = popni (loadMaze "maps/1.txt")
eff2 = popni (loadMaze "maps/2.txt")

loadManager :: Manager
loadManager = ( Manager eff 0 0 0 0 defaultDelayTime )
--loadManager = ( Manager e 0 0 0 0 defaultDelayTime )

updateControlledPlayer :: Key -> Manager -> Manager
updateControlledPlayer k ( Manager (State lab (jogadore:fs) l) x y z w d ) = Manager (State lab (jognovo:fs) l) x y z w d
    where Pacman (PacState (idj,cordp,v,ori,p,vidas) time boca es) = jogadore
          jognovo = Pacman (PacState (idj,cordp,v,oriNova,p,vidas) time boca es)
          oriNova = nova_ori k

nova_ori :: Key -> Orientation
nova_ori k 
    | k == KeyUpArrow    = U
    | k == KeyDownArrow  = D
    | k == KeyLeftArrow  = L 
    | k == KeyRightArrow = R

updateScreen :: Window  -> ColorID -> Manager -> Curses ()
updateScreen w a man =
                  do
                    updateWindow w $ do
                      clear
                      setColor a
                      moveCursor 0 0 
                      drawString $ show (state man)
                    render
     
currentTime :: IO Integer
currentTime = fmap ( round . (* 1000) ) getPOSIXTime

updateTime :: Integer -> Manager -> Manager
updateTime now man = man {delta = (now - before man)}

resetTimer :: Integer -> Manager -> Manager
resetTimer now man = man {delta = 0, before = now} 

nextFrame :: Integer -> Manager -> Manager
nextFrame now man = let update = (resetTimer now man)
                    in update { state= (passTime (step man) ((state man))) , step = (step man) +1 }

loop :: Window -> Manager -> Curses ()
loop w man@(Manager s pid step bf delt del ) = do 
  color_schema <- newColorID ColorBlue ColorWhite  10
  now <- liftIO  currentTime
  updateScreen w  color_schema man
  if ( delt > del )
    then loop w $ nextFrame now man
    else do
          ev <- getEvent w $ Just 0
          case ev of
                Nothing -> loop w (updateTime now man)
                Just (EventSpecialKey arrow ) -> loop w $ updateControlledPlayer arrow (updateTime now man)
                Just ev' ->
                  if (ev' == EventCharacter 'q')
                    then return ()
                    else loop w (updateTime now man)

main :: IO ()
main =
  runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    w <- defaultWindow
    loop w loadManager

