{-Clase tipos y funciones 13/4 -}

pair2List :: (a,a) -> [a]
pair2List (x,y) = x:y:[]

tryGet :: [a] -> Int -> a -> a
tryGet lista indice valor
    |indice >= 0 && (length lista) > indice = lista !! indice
    |otherwise = valor

tryGets :: [[a]] -> Int -> a -> a
tryGets [] _ valor = valor
tryGets (x:xs) indice valor
    |indice >= 0 && (length x) > indice = x !! indice
    |indice >= 0 && (length x) <= indice = tryGets xs indice valor
    |otherwise = valor

{-splitInHalf :: [a] -> (a,a)
splitInHalf x
   | length x `mod` 2 == 0 = ((take (length x `div` 
   	2) x) ,(drop (length x `div` 2) x))
   | otherwise = ((take (length x  `div` 2) x) ,(drop ((length x  `div` 2) + 1) x))-}

crossover :: [a] -> [a] -> Int -> ([a],[a])
crossover x y num = ((take num x) ++ (drop num y) , (take num y) ++ (drop num x))

swap :: [a] -> Int -> Int -> [a]
swap x num1 num2
 | num1<0 || num1>= length x || num2<0 || num2>= length x = error "Error!"
 | num1 == num2 = x
 | otherwise = a ++ [x!!num1] ++ b
 where a= (take num2 ((take num1 x) ++ [x !! num2] ++ (drop (num1+1) x)))
       b= (drop (num2+1) ((take num1 x) ++ [x !! num2] ++ (drop (num1+1) x)))


{-diceByValueInt :: [Int] -> (Int,Int,Int,Int,Int,Int) -> (Int,Int,Int,Int,Int,Int)
diceByValueInt [] (uno,dos,tres,cuatro,cinco,seis) = (uno,dos,tres,cuatro,cinco,seis)
diceByValueInt (x:xs) (uno,dos,tres,cuatro,cinco,seis)
  | x == 1 = diceByValueInt xs ((uno+1),dos,tres,cuatro,cinco,seis)
  | x == 2 = diceByValueInt xs (uno,(dos+1),tres,cuatro,cinco,seis)
  | x == 3 = diceByValueInt xs (uno,dos,(tres+1),cuatro,cinco,seis)
  | x == 4 = diceByValueInt xs ((uno),dos,tres,(cuatro+1),cinco,seis)
  | x == 5 = diceByValueInt xs ((uno),dos,tres,cuatro,(cinco+1),seis)
  | x == 6 = diceByValueInt xs ((uno),dos,tres,cuatro,cinco,(seis+1))
  | otherwise = error "Utilice un dado cubico!"



diceByValue :: (Int,Int,Int,Int,Int) -> (Int,Int,Int,Int,Int,Int)
diceByValue (a,b,c,d,e) = diceByValueInt[a,b,c,d,e] (0,0,0,0,0,0)
 -}

nOf :: Int -> a -> [a]
nOf n v
  | n <= 0 = []
  | otherwise = [v] ++ nOf (n-1) v

updateTriplet :: (a,a,a) -> Int -> a -> (a,a,a)
updateTriplet (x,y,z) num valor
  | num <1 || num>3 = (x,y,z)
  | num == 1 = (valor,y,z)
  | num == 2 = (x,valor,z)
  | num == 3 = (x,y,valor)

reverseX :: [a] -> [a]
reverseX [] = []
reverseX (x:xs) = (reverseX xs)++[x]

clamp :: Int -> Int -> Int -> Int
clamp value min max 
 | value < min = min
 | value > max = max
 | otherwise = value

fun [] ys = ys
fun (x:xs) ys = x:(fun xs ys)


{-Escribir una función de Haskell que tome una lista de enteros (no necesariamente ordenada) 
y retorne el menor número entero mayor que no esté en la lista y 
que sea mayor al mínimo número de la lista. Si la lista está vacía se debe arrojar un error.

minAfterMin :: [Int] -> Int
Por ejemplo:

(minAfterMin [1,2,3]) == 4 
(minAfterMin [1,3,7,5,6]) == 2
(minAfterMin [17]) == 18
(minAfterMin [77,700,707,70,7,770]) == 8
(minAfterMin [0..10]) == 11 -}


tieneSucesor :: Int -> [Int] -> Bool
tieneSucesor x [] = False
tieneSucesor x (y:ys)
   | x+1 == y = True
   | otherwise = tieneSucesor x ys

delete :: Int -> [Int] ->[Int]
delete x [] = []
delete x (y:ys)
   | x == y = ys
   | otherwise = [y] ++ delete x ys

minAfterMin :: [Int] -> Int
minAfterMin [] = error"Lista vacia"
minAfterMin xs
   | tieneSucesor m xs = minAfterMin (delete m xs)
   | otherwise = m+1
   where m = minimum xs


changeElem :: [a] -> Int -> a -> [a]
changeElem lista indice elem
 | indice < 0 || indice >= length  lista = error"El indice no es valido"
 | otherwise = (take indice lista) ++ [elem] ++ (drop (indice+1) lista)

anda :: Int -> (Int)
anda x = (x)

{-g :: [a] -> [a] 
g [] = []
g [x] = [x]
g (x:y:xs) = f (y:xs) ++ [x] ++ []

f :: (Eq a)=>[a] -> a -> Bool
f (x:xs) a 
     | a == x = True
     | otherwise = f xs a 
f [] a = False
-}
x :: [a] -> [a]
x (x:z:xs) = xs
x _ = []
inverso :: [a] -> [a] 
inverso [] = []
inverso [x] = [x]
inverso (x:y:xs) = inverso (y:xs) ++ [x] ++ []


capicua :: (Eq a) => [a] -> Bool
capicua n = n == inverso n


tupleMax :: [Int] -> [(Int, Int)]
tupleMax [] = []
tupleMax x = tupleMaxInt x (maximum x)

tupleMaxInt :: [Int] -> Int -> [(Int,Int)]
tupleMaxInt [] _ = []
tupleMaxInt (x:xs) max = [] ++ [(x,max)] ++ tupleMaxInt xs max


isDigit:: Char -> Bool
isDigit x = x `elem` ['0'..'9']

filterDigits :: String -> String
filterDigits x = filter isDigit x

data WeekDay = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Enum, Show)

nextDay :: WeekDay -> WeekDay
nextDay Sunday = Monday
nextDay a = succ a

previousDay :: WeekDay -> WeekDay
previousDay Monday = Sunday
previousDay a = pred a



{-Se hace con un toFeet en dos lineas mongolico-}

data UKLength = Yards Double | Feet Double | Inches Double deriving (Show)

instance Eq UKLength where
    (Yards x) == (Yards y) = x == y
    (Feet x) == (Feet y) = x == y
    (Inches x) == (Inches y) = x == y
    (Yards x) == (Feet y) = x == 3/y
    (Yards x) == (Inches y) = x == 36/y
    (Feet x) == (Yards y) = x/3 == y
    (Feet x) == (Inches y) = x == y/12
    (Inches x) == (Feet y) = x/12 == y
    (Inches x) == (Yards y) = x/36 == y


data NTree a = Node a [NTree a] | Empty deriving (Eq)

instance (Show a) => Show (NTree a) where
    show (Empty) = "()"
    show (Node x []) = "("++ (show (x)) ++")"
    show (Node x ts) = "(" ++ (show (x)) ++ " " ++ concat( map show ts ) ++ ")"

{-
type Casillero = (Int , Int, Int, Int)
type Tablero = (Casillero, Casillero, Casillero, Casillero)


sudoxu2x2_init :: Tablero
sudoku2x2_init= ((0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0))

sudoku2x2_nexts :: Tablero -> [Tablero]
sudoku2x2_nexts (a,b,c,d) = [(a2,b,c,d) | a2 <- sudoku2x2_nexts' a] ++ [(a,b2,c,d) | b2 <- sudoku2x2_nexts' b] ++ [(a,b,c2,d) | c2 <- sudoku2x2_nexts' c] ++ [(a,b,c,d2) | d2 <- sudoku2x2_nexts' d]

sudoku2x2_nexts' :: Casillero -> [Casillero]
sudoku2x2_nexts' (a,b,c,d)
   | a == 0 = elemChange 1 (a,b,c,d)
   | a == 0 && b == 0 = (elemChange 1 (a,b,c,d)) ++ (elemChange 2 (a,b,c,d))
   | a == 0 && b == 0 && c == 0 = (elemChange 1 (a,b,c,d)) ++ (elemChange 2 (a,b,c,d)) ++ (elemChange 3 (a,b,c,d))
   | a == 0 && b == 0 && c == 0 && d == 0 = (elemChange 1 (a,b,c,d)) ++ (elemChange 2 (a,b,c,d)) ++ (elemChange 3 (a,b,c,d)) ++ (elemChange 4 (a,b,c,d))
   | otherwise = []

sudoku2x2_target :: Tablero -> Bool
sudoku2x2_target ((a,b,c,d),(e,f,g,h),(i,j,k,l),(m,n,o,p)) = True


class SearchProblem a where
    estadoInicial :: a
    funcionSucesores :: a -> [a]
    testObjetivo :: a -> Bool

instance SearchProblem sudoku2x2 x where
    estadoInicial = sudoku2x2_init
    funcionSucesores x = sudoku2x2_nexts x
    funcionObjetivo x = sudoku2x2_target x
-}



{-TaTeTi PreProyect-}

data Chip = Cross | Circle | Emp deriving (Eq)
data Board = Board (Chip,Chip,Chip,Chip,Chip,Chip,Chip,Chip,Chip)

instance Show Chip where
    show Cross = "X"
    show Circle = "O"
    show Emp = " "

instance Show Board where
    show (Board(a,b,c,d,e,f,g,h,i)) = (show a) ++ "|" ++ (show b) ++ "|" ++ (show c) ++ "\n----\n" ++ (show d) ++ "|" ++ (show e) ++ "|" ++ (show f) ++ "\n----\n" ++ (show g) ++ "|" ++ (show h) ++ "|" ++ (show i)

inicialTateti :: Board
inicialTateti = Board(Emp,Emp,Emp,Emp,Emp,Emp,Emp,Emp,Emp)