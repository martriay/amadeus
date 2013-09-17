import Data.Char

-----------
-- Notas --
-----------

-- Returns a set of notes depending on its congruence with 12
-- Devuelve un set de notas dependiendo de su congruencia con 12, lo que determina si funciona con sostenidos o bemoles 
notas :: Int -> [[Char]]
notas x = [ n !! caso | n <- todas ]
  where todas = [["Do","Do"],["Do#","Reb"],["Re","Re"],["Re#","Mib"],["Mi","Mi"],["Fa","Fa"],["Fa#","Solb"],["Sol","Sol"],["Sol#","Lab"],["La","La"],["La#","Sib"],["Si","Si"]]
        caso
          | elem (mod x 12) [2,4,7,9,11] = 0
          | otherwise = 1

-- Maps every note to its numeric equivalent
-- Mapea cada nota con su equivalente numerico
nota :: [Char] -> Int
nota n
  | x ["do","si#"]   = 0
  | x ["do#","reb"]  = 1
  | x ["re"]         = 2
  | x ["re#","mib"]  = 3
  | x ["mi","fab"]   = 4
  | x ["fa","mi#"]   = 5
  | x ["fa#","solb"] = 6
  | x ["sol"]        = 7
  | x ["sol#","lab"] = 8
  | x ["la"]         = 9
  | x ["la#","sib"]  = 10
  | x ["si","dob"]   = 11
  | otherwise        = 12
  where x = elem $ map toLower n

-- Calculates the ionan mode basic structure
-- Calcula la estructura basica del modo jonico
escala :: [Int]
escala = scanl (+) 0 [tono, tono, semitono, tono, tono, tono]
  where semitono = 1
        tono = 2

-------------
-- Escalas --
-------------

-- Returns a set of notes depending on its tonal center, modificating the particular case of Gb which breaks everything
-- Devuelve un set de notas dependiendo del centro tonal, modificando el caso particular de Solb para que no repita nombres de notas
tonalidad :: Int -> [[Char]]
tonalidad x = [ cycle (notas x) !! ((caso n) + x) | n <- escala ]
  where caso n
          | x == 6 && n == 5 = 6
          | otherwise = n

-- Shifts a tonality setting the key note on "b"
-- Se corre una tonalidad hasta colocar el centro tonal en "b"
-- el modo "a" debe correr la tonalidad tantas posiciones como valor haya en el indice "a" de "escala"
modo :: Int -> [Char] -> [[Char]]
modo a b = [ cycle corrimiento !! (n + a) | n <- [0..6] ]
  where corrimiento = tonalidad $ c + 12 - (escala !! a)
        c = nota b

jonico    = modo 0
dorico    = modo 1
frigio    = modo 2
lidio     = modo 3
mixolidio = modo 4
eolico    = modo 5
locrio    = modo 6

mayor = jonico
menor = eolico

-------------
-- Acordes --
-------------

estructura :: Int -> [Int]
estructura n = take n [0,2..]

-- Takes a scale and builds its basic chords
-- Construye los acordes basicos de una escala dada
acordes x = [ triada e | e <- [0..6] ]
  where triada e = [ cycle x !! (n+e) | n <- estructura 3 ]
