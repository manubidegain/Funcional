{- PROYECTO PROGRAMACION FUNCIONAL -}
{- Bruno Garcia , Lucas Garcia, Facundo Petre , Juan Bidegain ##-}

data Col = C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 deriving (Eq, Show, Ord, Bounded)
data Row = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 deriving (Eq, Show, Ord, Bounded) 
data Coord = Coord Col Row deriving (Eq, Show, Ord)
data ShieldPieces = Shield | King deriving (Eq, Show, Ord)
data SwordPieces = Sword deriving (Eq, Show, Ord)
data TablutPlayer = ShieldPlayer | SwordPlayer deriving (Eq, Show, Enum)
data Pieces = ShielPieces | SwordPieces  deriving (Eq, Show, Ord)
data Box = Pieces Coord | Empty Coord  deriving (Eq, Show, Ord)
data Board = Board [Box] deriving (Eq, Show, Ord)

 
