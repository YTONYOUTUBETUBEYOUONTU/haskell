parOImpar :: Int -> String
parOImpar a = if a `mod` 2 == 0 then "par" else "impar"

main :: IO ()
main = do
    putStrLn "Ingrese un número:"
    num <- readLn :: IO Int
    putStrLn $ "El número " ++ show num ++ " es " ++ parOImpar num