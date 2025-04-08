suma :: Int -> Int -> Int -> Int
suma a b c = a + b + c

division :: Int -> Int -> Maybe Float
division a 0 = Nothing
division a b = Just (fromIntegral a / fromIntegral b)

main :: IO ()
main = do
    let resultado = suma 5 10 2
    putStrLn $ "La suma de " ++ show 5 ++ ", " ++ show 10 ++ " y " ++ show 2 ++ " es " ++ show resultado
    let resDiv = division 10 0
    case resDiv of
        Nothing -> putStrLn "ERROR: No se puede dividir por cero"
        Just resultado -> putStrLn ("La división de 10 entre 2 es " ++ show resultado)