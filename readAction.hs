import Data.List.Split
import Data.Char

userInput :: String -> (Int,Int)
userInput str = (digitToInt((elemList !! 0)!! 0) , digitToInt((elemList !! 1)!! 0))
  where
    noBrackets = filter (\ x -> not (x `elem` "()")) str
    elemList = splitOn "," noBrackets


getUserInteraction= do  
    putStrLn "Ingrese su movimiento en el siguiente formato: (columna,fila)"  
    movement <- getLine  
    putStrLn ("Se ha movido " ++ show (userInput movement))
