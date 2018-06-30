{- Tablut -------------------------------------------------------------------------------------------
 
Plantilla de código para el proyecto del curso de 2018 de _Programación Funcional_ para las carreras 
de Ingeniería y Licenciatura en Informática de la FIT (UCU).
Los docentes no garantizan que este código esté libre de errores. De encontrar problemas, por favor
reportarlos a la cátedra.

Por Leonardo Val.
-}
module Tablut where

import Data.Maybe (fromJust, listToMaybe)
import Data.List (elemIndex)
import System.Random

{- Es posible que el paquete System.Random no esté disponible si se instaló el core de la Haskell 
Platform en el sistema. Para instalarlo, ejecutar los siguientes comandos:

> cabal update
> cabal install random

La herramienta `cabal` es un manejador de paquetes usado por la plataforma Haskell. Debería estar 
disponible junto con el `ghci`.

-}

{-- Lógica de juego --------------------------------------------------------------------------------

Funciones de marca sin ninguna implementación útil. Reemplazar por el código apropiado o por imports
a los módulos necesarios.
-}

data TablutPlayer = ShieldPlayer | SwordPlayer deriving (Eq, Show)
data Square = Sword | Shield | King | Empty  deriving (Eq)
data Board = Board [[Square]] deriving (Eq)
data TablutGame = TablutGame Board TablutPlayer TablutPlayer deriving (Eq)
data TablutAction =TablutAction (Int,Int) (Int,Int) deriving (Eq, Show)

instance Show Square where
    show Sword = "T"
    show Shield = "O"
    show King = "$"
    show Empty = " "




instance Show Board where
    show (Board [x]) = (returnRow x)
    show (Board (x:xs)) =(returnRow x) ++ "\n" ++ show (Board xs)  

instance Show TablutGame where
    show (TablutGame a b c) = " A B C D E F G H I\n" ++ show a ++ "\n \nCurrent: " ++ show b


returnRow :: [Square] -> String
returnRow x = foldl(\a b -> a++show b++"|") "|" x

beginning :: TablutGame
beginning = TablutGame (Board (map (\str -> map func str) board)) ShieldPlayer SwordPlayer
   where
   func 'A' = Empty
   func 'B' = Sword
   func 'C' = Shield
   func 'D' = King
   board = ["AAABBBAAA", "AAAABAAAA", "AAAACAAAA", "BAAACAAAB", "BBCCDCCBB", "BAAACAAAB", "AAAACAAAA", "AAAABAAAA","AAABBBAAA"]

-- actions :: TablutGame -> [(TablutPlayer, [TablutAction])]
-- actions (TablutGame (Board x) p1 p2) = error "Metodo no implementado" 
-- actions _ = [(ShieldPlayer, [TablutAction (Board [[]])]), (SwordPlayer, [])] 

isMyPiece :: Square -> TablutPlayer -> Bool
isMyPiece a SwordPlayer = a == Sword
isMyPiece a ShieldPlayer = a == Shield || a == King

whatsThere :: Board -> (Int, Int) -> Square
whatsThere (Board b) (x, y) = (b !! y) !! x

notEmpty :: Board -> (Int,Int) -> Bool
notEmpty (Board b) (x, y) = whatsThere (Board b) (x, y) /= Empty

gameToBoard :: TablutGame -> Board
gameToBoard (TablutGame brd _ _)= brd

boardToCoords :: Board -> [(Int,Int)]
boardToCoords (Board x) = filter (notEmpty (Board x)) coords
    where
    coords = foldl (\a b -> a++(zip (replicate 9 b) [0..8])) [] [0..8]

-- next :: TablutGame -> (TablutPlayer, TablutAction) -> TablutGame
-- next game (player,(action (w,x) (y,z))) = TablutGame (Board (map())
-- next _ _ = beginning

-- result :: TablutGame -> [(TablutPlayer, Int)]
-- result (TablutGame f) = if f then [(ShieldPlayer, 1), (SwordPlayer, (-1))] else [] --TODO

showBoard :: TablutGame -> String
showBoard g = show g



-- showAction :: TablutAction -> String
-- showAction a = show a --TODO
   
-- readAction :: String -> TablutAction
-- readAction = read --TODO

-- activePlayer :: TablutGame -> Maybe TablutPlayer
-- activePlayer g = listToMaybe [p | (p, as) <- actions g, not (null as)]

{-- Match controller -------------------------------------------------------------------------------

Código de prueba. Incluye una función para correr las partidas y dos agentes: consola y aleatorio.

-}
type TablutAgent = TablutGame -> IO (Maybe TablutAction)

{- La función ´runMatch´ corre la partida completa a partir del estado de juego dado, usando los dos 
agentes dados. Retorna una tupla con los puntajes (score) finales del juego.
-}
-- runMatch :: (TablutAgent, TablutAgent) -> TablutGame -> IO [(TablutPlayer, Int)]
-- runMatch ags@(ag1, ag2) g = do
--    putStrLn (showBoard g)
--    case (activePlayer g) of
--       Nothing -> return $ result g
--       Just p -> do
--          let ag = [ag1, ag2] !! (fromJust $ elemIndex p [ShieldPlayer, SwordPlayer])
--          move <- ag g
--          runMatch ags (Tablut.next g (p, fromJust move))

{- La función ´runOnConsole´ ejecuta toda la partida a partir del estado inicial usando dos agentes
de consola.
-}
-- runOnConsole :: IO [(TablutPlayer, Int)]
-- runOnConsole = do
--    runMatch (consoleAgent ShieldPlayer, consoleAgent SwordPlayer) beginning

{- El agente de consola ´consoleAgent´ muestra el estado de juego y los movimientos disponibles por
consola, y espera una acción por entrada de texto.
-}
-- consoleAgent :: TablutPlayer -> TablutAgent
-- consoleAgent player state = do
--    let moves = fromJust $ lookup player (actions state)
--    if null moves then do
--       putStrLn "No moves!"
--       getLine
--       return Nothing
--    else do
--       putStrLn ("Select one move:" ++ concat [" "++ show m | m <- moves])
--       line <- getLine
--       let input = readAction line
--       if elem input moves then return (Just input) else do 
--          putStrLn "Invalid move!"
--          consoleAgent player state

-- randomAgent :: TablutPlayer -> TablutAgent
-- randomAgent player state = do
--     let moves = fromJust $ lookup player (actions state)
--     if null moves then do
--        putStrLn "No moves!"
--        return Nothing
--     else do
--        i <- randomRIO (0, (length moves) - 1)
--        return (Just (moves !! i))
