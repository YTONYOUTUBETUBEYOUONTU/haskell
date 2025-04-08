import System.IO

-- Algoritmo quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort menores ++ [x] ++ quicksort mayores
  where
    menores = filter (< x) xs
    mayores = filter (>= x) xs

-- Función para leer datos desde consola
leerDatosConsola :: IO [String]
leerDatosConsola = do
    putStrLn "Ingrese un dato (deje vacío para terminar):"
    dato <- getLine
    if dato == ""
        then return []
        else do
            resto <- leerDatosConsola
            return (dato : resto)

main :: IO ()
main = do
    let archivoEntrada = "input.txt"
    let archivoSalida = "output.txt"

    -- Leer datos desde archivo usando lambda
    datosArchivo <- (\archivo -> fmap lines (readFile archivo)) archivoEntrada

    -- Leer datos desde consola
    datosConsola <- leerDatosConsola

    -- Combinar y ordenar
    let todosLosDatos = datosArchivo ++ datosConsola
    let datosOrdenados = quicksort todosLosDatos

    -- Escribir datos usando lambda
    (\archivo datos -> writeFile archivo (unlines datos)) archivoSalida datosOrdenados

    putStrLn "Datos combinados y ordenados escritos en output.txt"

    return ()