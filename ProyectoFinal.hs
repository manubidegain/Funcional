{- PROYECTO PROGRAMACION FUNCIONAL -}
{- Bruno Garcia , Lucas Garcia, Facundo Petre , Juan Bidegain ##-}

data TablutPlayer = ShieldPlayer | SwordPlayer deriving (Eq, Show, Enum)
data Square = Sword | Shield | King | Empty  deriving (Eq, Ord)
data Board = Board [[Square]] deriving (Eq, Ord)
data TablutGame = TablutGame Board TablutPlayer TablutPlayer deriving (Eq)
data TablutAction = TablutAction [Board] deriving (Eq, Show, Ord)

tablero :: [String]
tablero = tab ++ (reverse (take 4 tab)) where tab = ["AAABBBAAA", "AAAABAAAA", "AAAACAAAA", "BAAACAAAB", "BBCCDCCBB"] 

beginning :: TablutGame
beginning = TablutGame (Board (map (\str -> map func str) tablero)) ShieldPlayer SwordPlayer
   where
   func 'A' = Empty
   func 'B' = Sword
   func 'C' = Shield
   func 'D' = King

returnRow :: [Square] -> String
returnRow x = foldl(\a b -> a++show b++"|") "|" x


instance Show Square where
    show Sword = "T"
    show Shield = "O"
    show King = "$"
    show Empty = " "

instance Show Board where
    show (Board [x]) = (returnRow x)
    show (Board (x:xs)) =(returnRow x) ++ "\n" ++ show (Board xs)  

instance Show TablutGame where
    show (TablutGame a b c) =show a







 
