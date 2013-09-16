import Data.Char

------------------
-- Definiciones --
------------------

-- Returns a set of notes depending on its congruence with 12
-- Devuelve un set de notas dependiendo de su congruencia con 12, lo que determina si funciona con sostenidos o bemoles 
notas :: Int -> [[Char]]
notas x = [ n !! caso | n <- todas ]
  where todas = [["Do","Do"],["Do#","Reb"],["Re","Re"],["Re#","Mib"],["Mi","Mi"],["Fa","Fa"],["Fa#","Solb"],["Sol","Sol"],["Sol#","Lab"],["La","La"],["La#","Sib"],["Si","Si"]]
        caso
          | elem (mod x 12) [2,4,7,9,11] = 0
          | otherwise = 1

-- Maps every note with its numeric equivalent
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
estructura :: [Int]
estructura = scanl (+) 0 base
  where base = [tono, tono, semitono, tono, tono, tono]
        semitono = 1
        tono = 2

-------------------
-- Funcionalidad --
-------------------

-- Returns a set of notes depending on its tonal center, modificating the particular case of Gb which breaks everything
-- Devuelve un set de notas dependiendo del centro tonal, modificando el caso particular de Solb para que no repita nombres de notas
tonalidad :: Int -> [[Char]]
tonalidad x = [ cycle (notas x) !! ((caso n) + x) | n <- estructura ]
  where caso n
          | x == 6 && n == 5 = 6
          | otherwise = n

-- Shifts a tonality setting the key note on "y"
-- Se corre una tonalidad hasta colocar el centro tonal en "y"
-- el modo "x" debe correr la tonalidad tantas posiciones como valor haya en el indice "x" de "estructura"
escala :: Int -> Int -> [[Char]]
escala x y = [ cycle (tonalidad $ y + 12 - (estructura !! x)) !! (n + x) | n <- [0..6] ]

modo :: Int -> [Char] -> [[Char]]
modo n x = escala n (nota x)

jonico    = modo 0
dorico    = modo 1
frigio    = modo 2
lidio     = modo 3
mixolidio = modo 4
eolico    = modo 5
locrio    = modo 6

mayor = jonico
menor = eolico
