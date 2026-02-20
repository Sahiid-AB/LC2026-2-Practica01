module Practica01 where

--TIPOS ALGEBRAICOS

--Ejercicio 1
data Shape = Circle Float | --representa el radio
            Square Float | --representa un lado
            Rectangle Float Float| --representa base y altura
            Triangle Float | --representa un lado
            Trapeze Float Float Float --representa base mayor, base menor y altura
            deriving (Show, Eq)

--Funcion que calcula el area de las figuras
area :: Shape -> Float
area (Circle radio) = pi * radio * radio
area (Square lado) = lado * lado
area (Rectangle base altura) = base * altura
area (Triangle lado) = (sqrt 3 / 4) * (lado * lado)
area (Trapeze base_M base_m altura) = ((base_M + base_m) * altura ) / 2 


--Funcion que calcula el perimetro de las figuras
perimeter :: Shape -> Float
perimeter (Circle radio) = 2 * radio * pi
perimeter (Square lado) = 4 * lado
perimeter (Rectangle base altura) = (2 * base) + (2 * altura)
perimeter (Triangle lado) = (lado * 3)
perimeter (Trapeze base_M base_m altura) = base_M + base_m + (2*lado)
                                                where
                                                -- Funciones/Variables auxiliares para el Ejercicio 1 (Trapecio)
                                                -- Calculan el lado inclinado asumiendo un trapecio isósceles usando el Teorema de Pitágoras.
                                                -- Restamos las bases para obtener la base de 2 triangulos
                                                -- Dividimos entre 2 para calcular la hipotenusa de uno sol
                                                -- Aplicamos teorema de Pitagóras lado = sqrt(a^2+b^2)
                                                baseTriangulo = (base_M - base_m) / 2
                                                lado = sqrt ((baseTriangulo *baseTriangulo) + (altura * altura))


--Ejercicio 2 (Les toca arreglar el sinonimo)
type Point = (Float, Float)

-- Funcion para calcular la distancia entre dos puntos
distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2)= sqrt (((x2-x1)*(x2-x1)) + ((y2-y1)*(y2-y1)))

--Funcion para calcular la distancia de un punto al origen
from0 :: Point -> Float
from0 (x,y) = distance (x,y) (0,0)

--Ejercicio 3 
data Haskellium = Haskellium {
    name :: String,
    lastName1 :: String,
    lastName2 :: String,
    location :: Point,
    houseShape :: Shape
} deriving(Show)

--Funcion para regresar el hijo de dos Haskelliums dado su nombre
son :: Haskellium -> Haskellium -> String -> Haskellium
son padre1 padre2 nombreHijo = Haskellium {
    name = nombreHijo,
    lastName1 = lastName1 padre1,
    lastName2 = lastName1 padre2,
    location = location padre1,
    houseShape = houseShape padre1
} 

--Funcion para calcular las unidades para construir la casa de un Haskellium
houseCost :: Haskellium -> Float
houseCost padre = area (houseShape padre) + (perimeter (houseShape padre) * 2.5)

--Funcion para calcular el tiempo que le toma a un Haskellium para llegar a su trabajo
timeToWork :: Haskellium -> Float
timeToWork casa = distancia / velocidad 
                    where
                    -- Funciones/Variables auxiliares para el Ejercicio 3
                    -- 'distancia' obtiene la distancia de la casa al origen.
                    -- 'velocidad' determina si viaja a 30 o 70 dependiendo de la distancia.
                    distancia = from0 (location casa)
                    velocidad = if distancia < 300 then 30 else 70
                    
                        
--LISTAS Y FUNCIONES
--Ejercicio 1
palindromo :: (Eq a) => [a] -> Bool
palindromo xs =  xs == invertir xs 
                    where   
                    -- Función auxiliar para el Ejercicio 1 de Listas.
                    -- Recibe una lista y devuelve la misma lista con sus elementos en orden inverso.
                    invertir [] = []
                    invertir (x:ys) = invertir ys ++ [x] 


--Ejercicio 2
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ valor [] = valor
myFoldr funcion valor (x:xs) = funcion x (myFoldr funcion valor xs) 

--Ejercicio 3
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia [] = [[]]
conjuntoPotencia (x:xs) = [ x:ys | ys <- resto ] ++ resto
                            where 
                            -- Variable/Llamada auxiliar para el Ejercicio 3 de Listas.
                            -- 'resto' almacena el conjunto potencia del resto de la lista para evitar recalcularlo.
                            resto = conjuntoPotencia xs 

--ARBOLES

--Implementacion

data OneTwoTree a = Void 
                    | Node a (OneTwoTree a)
                    | Branch a (OneTwoTree a) (OneTwoTree a)
                      deriving (Show)

--Ejercicio 2
suma :: OneTwoTree Int -> Int
suma Void = 0
suma (Node valor hijo) = valor + suma hijo
suma (Branch valor hijoIzq hijoDer ) =  valor + suma hijoIzq + suma hijoDer
