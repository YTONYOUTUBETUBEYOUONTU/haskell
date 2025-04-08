
main :: IO ()
main = do
    writeFile "archivo.txt" "Quien eres??"
    contenido <- readFile "archivo.txt"

    putStrLn contenido
