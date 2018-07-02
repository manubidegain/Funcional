actions :: TablutGame -> [(TablutPlayer, [TablutAction])]
actions tg@(TablutGame b active _) = [(ShieldPlayer, movs ShieldPlayer), (SwordPlayer, movs SwordPlayer)]
--actions _ = [(ShieldPlayer, [TablutAction b), (SwordPlayer, [])]
    where
        movs p = if p == active then possibleActions tg else []

possibleActions :: TablutGame -> [TablutAction]
possibleActions (TablutGame (Board rows) active _) = foldl (\a b -> possiblePieceActions (Board rows) b ++ a) [] myPieces
    where
        myPieces = filter (isMyPiece active . whatsThere (Board rows) ) (boardToCoords (Board rows))

possiblePieceActions :: Board -> (Int, Int) -> [TablutAction]
possiblePieceActions b (x, y) = tryLeft b x (x,y) ++ tryRight b x (x, y) ++ tryUp b y (x, y) ++ tryDown b y (x, y)

tryLeft, tryRight, tryUp, tryDown :: Board -> Int -> (Int, Int) -> [TablutAction]
tryLeft b x0 (x, y) = if x > 0 && isEmpty b (x-1, y) then TablutAction (x0, y) (x-1, y) : tryLeft b x0 (x-1, y) else []
tryRight b x0 (x, y) = if x < 8 && isEmpty b (x+1, y) then TablutAction (x0, y) (x+1, y) : tryRight b x0 (x+1, y) else []
tryUp b y0 (x, y) = if y > 0 && isEmpty b (x, y-1) then TablutAction (x, y0) (x, y-1) : tryUp b y0 (x, y-1) else []
tryDown b y0 (x, y) = if y < 8 && isEmpty b (x, y+1) then TablutAction (x, y0) (x, y+1) : tryDown b y0 (x, y+1) else []




isMyPiece :: TablutPlayer -> Square -> Bool
isMyPiece SwordPlayer a = a == Sword
isMyPiece ShieldPlayer a = a == Shield || a == King