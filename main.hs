import Data.Char

-- Devuelve un set de notas dependiendo de su resto modulo 12, determinando si es bemol, sostenido o becuadro
notas :: Int -> [[Char]]
notas x = [ n !! caso | n <- todas ]
  where todas = [["Do","Do"],["Do#","Reb"],["Re","Re"],["Re#","Mib"],["Mi","Mi"],["Fa","Fa"],["Fa#","Solb"],["Sol","Sol"],["Sol#","Lab"],["La","La"],["La#","Sib"],["Si","Dob"]]
        caso
          | elem (mod x 12) [0,2,4,7,9,11] = 0
          | otherwise = 1

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

-- Calcula la estructura basica del modo jonico
escala :: [Int]
escala = scanl (+) 0 [tono, tono, semitono, tono, tono, tono]
  where semitono = 1
        tono = 2

-- Devuelve un set de notas dependiendo del centro tonal, modificando el caso particular de Solb para que no repita nombres de notas
tonalidad :: Int -> [[Char]]
tonalidad x = [ cycle (notas x) !! ((caso n) + x) | n <- escala ]
  where caso n
          | x == 6 && n == 5 = 6
          | otherwise = n


-- Se corre una tonalidad hasta colocar el centro tonal en $b
-- el modo "a" debe correr la tonalidad tantas posiciones como valor haya en el indice $a de $escala
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

estructura :: Int -> [Int]
estructura n = take n [0,2..]

-- Construye los acordes basicos de una escala dada
acordes x = [ triada e | e <- [0..6] ]
  where triada e = [ cycle x !! (n+e) | n <- estructura 3 ]
