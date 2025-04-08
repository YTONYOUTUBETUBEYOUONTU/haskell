-- Definimos un tipo de datos algebraico para representar una lista de elementos de diferentes tipos
data MiLista = ListaEnteros [Int] | ListaCadenas [String] | ListaDecimales [Double]

-- Definimos una función para calcular la longitud de una lista
longitud :: MiLista -> Int
longitud (ListaEnteros xs) = length xs
longitud (ListaCadenas xs) = length xs
longitud (ListaDecimales xs) = length xs

-- Definimos una función para mostrar una lista
mostrar :: MiLista -> IO ()
mostrar (ListaEnteros xs) = print xs
mostrar (ListaCadenas xs) = print xs
mostrar (ListaDecimales xs) = print xs

-- Definimos una función para leer una lista de enteros desde la entrada estándar
leerLista :: IO MiLista
leerLista = do
  putStrLn "Ingrese una lista de enteros:"
  xs <- readLn :: IO [Int]
  return (ListaEnteros xs)

-- Definimos una función para leer una lista de cadenas desde la entrada estándar
leerListaCadenas :: IO MiLista
leerListaCadenas = do
  putStrLn "Ingrese una lista de cadenas:"
  xs <- readLn :: IO [String]
  return (ListaCadenas xs)

-- Definimos una función para leer una lista de números decimales desde la entrada estándar
leerListaDecimales :: IO MiLista
leerListaDecimales = do
  putStrLn "Ingrese una lista de números decimales:"
  xs <- readLn :: IO [Double]
  return (ListaDecimales xs)

-- Definimos la función principal
main :: IO ()
main = do
  putStrLn "Seleccione una opción:"
  putStrLn "1. Leer una lista de enteros"
  putStrLn "2. Leer una lista de cadenas"
  putStrLn "3. Leer una lista de números decimales"
  opcion <- readLn :: IO Int
  case opcion of
    1 -> do
      lista <- leerLista
      putStrLn $ "La longitud de la lista es: " ++ show (longitud lista)
      mostrar lista
    2 -> do
      lista <- leerListaCadenas
      putStrLn $ "La longitud de la lista es: " ++ show (longitud lista)
      mostrar lista
    3 -> do
      lista <- leerListaDecimales
      putStrLn $ "La longitud de la lista es: " ++ show (longitud lista)
      mostrar lista
    _ -> putStrLn "Opción inválida"