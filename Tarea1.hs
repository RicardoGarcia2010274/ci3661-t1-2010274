-- =====================================================================
--  CI-3661 — Tarea 1 (Haskell) — Septiembre–Diciembre 2026
--
--  Nombre: Ricardo Anael Garcia Rengifo
--  Carnet: 20-10274
--
--  Declaración de uso de herramientas de IA (obligatoria, ver enunciado):
--
-- =====================================================================
--  No cambies el nombre del módulo ni la lista de exportación, ni las
--  firmas de las funciones. Puedes agregar todas las funciones auxiliares
--  que quieras. El único import permitido es Data.Char.
-- =====================================================================
module Tarea1
  ( esPalindromo
  , mayorYMenor
  , leerHora
  , duracion
  , promedioSobreUmbral
  , movimientosCaballo
  , enDosSaltos
  , miMap
  , miFilter
  , miFoldl
  ) where

--Use esto en la pregunta A.1
import Data.Char (isAlphaNum, isDigit, toLower, digitToInt)

-- =====================================================================
-- Parte A: recursión explícita.
-- Prohibido: map, filter, foldr, foldl, listas por comprensión, reverse,
-- maximum, minimum, all, any y cualquier otra función de orden superior.
-- =====================================================================

-- A1

--Creamos la funcion que pueda invertir la cadena de texto
invertirString :: String -> String

-- Este es mi caso base
invertirString [] = []
-- este es mi caso recursivo
invertirString (x:xs) = invertirString xs ++ [x]


-- Creamos una funcion para pasar todo a minusculas
limpiar :: String -> String
limpiar [] = []
limpiar (x:xs)
  | isAlphaNum x = toLower x : limpiar xs
  | otherwise    = limpiar xs


esPalindromo :: String -> Bool

esPalindromo str = limpia == invertirString limpia
  where
    limpia = limpiar str
-- A2
-- Explica aquí, en dos o tres líneas, por qué tu solución recorre la
-- lista una sola vez: porque estoy viendo la lsita elemento a elemento mediante la funcion 
-- auxiliar recursiva go , actualizando el max y el min en una unica pasada. 

--
mayorYMenor :: Ord a => [a] -> Maybe (a, a)
mayorYMenor [] = Nothing
mayorYMenor (x:xs) = Just (go x x xs)
  where 
    go maxAcc minAcc  [] = (maxAcc, minAcc)
    go maxAcc minAcc (y:ys) = go (max maxAcc y) (min minAcc y) ys


-- A3

-- Codigo de la pregunta A3 
leerHora :: String -> Either String (Int, Int)
-- usando eso nos aseguramos que la cadena tenga exactamente cinco caracteres 

-- isDIgit verifica que los caracteres sean numericos 
leerHora [a, b, ':', c, d]
  | isDigit a && isDigit b && isDigit c && isDigit d =
      let hora    = read [a, b]
          minutos = read [c, d]
      --Aqui estoy verificando que los digitos no esten fuera de rango
      in if hora > 23      
           then Left "hora fuera de rango"
           else if minutos > 59
                  then Left "minutos fuera de rango"
                  else Right (hora, minutos)
  | otherwise = Left "formato inválido"
leerHora _ = Left "formato inválido"

duracion :: String -> String -> Either String Int

--uso funciones case anidadas, para encadenar valores de manera aislada
duracion s1 s2 = case leerHora s1 of
  Left err1 -> Left err1
  Right (h1, m1) -> case leerHora s2 of
    Left err2 -> Left err2
    Right (h2, m2) ->
      let t1   = h1 * 60 + m1
          t2   = h2 * 60 + m2
          diff = t2 - t1
      in Right (if diff < 0 then diff + 1440 else diff)

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1

--uso foldr que es bien interesante, ya que me permite reocorrer la lsita 
-- mientreas voy evaluando la funcion paso 
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral umbral xs =
  let (suma, cuenta) = foldr paso (0.0, 0) xs
      paso x (s, c)
        | x > umbral = (s + x, c + 1)
        | otherwise  = (s, c)

  --si no encuentra un elemento mayor, retoirna nothing 
  in if cuenta == 0
       then Nothing
       else Just (suma / fromIntegral cuenta)

-- B2
movimientosCaballo :: (Int, Int) -> [(Int, Int)]

--listo los pares (dr,dc), dde foirma que las cordenadas salgan en orden 
--lexicografico 

movimientosCaballo (r, c) =
  [ (r', c')
  | (dr, dc) <- [(-2, -1), (-2, 1), (-1, -2), (-1, 2), (1, -2), (1, 2), (2, -1), (2, 1)]
  , let r' = r + dr
  , let c' = c + dc
  , r' >= 1 && r' <= 8
  , c' >= 1 && c' <= 8
  ]

enDosSaltos :: (Int, Int) -> Int

--solamente la lsita de casillas alcanzadas tras el segundo salto
enDosSaltos pos = length sinDuplicados
  where
    -- Todos los destinos posibles tras 2 saltos
    segundoSalto = [ pos2 | pos1 <- movimientosCaballo pos, pos2 <- movimientosCaballo pos1 ]
    
    -- uso foldr para recorrer y sumar solo las posiciones pr las que no he pasado 
    sinDuplicados = foldr (\x acc -> if x `elem` acc then acc else x : acc) [] segundoSalto

-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap f = foldr (\x acc -> f x : acc) []

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter p = foldr (\x acc -> if p x then x : acc else acc) []

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl f z xs = foldr (\x k acc -> k (f acc x)) id xs z
