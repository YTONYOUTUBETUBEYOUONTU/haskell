cuadradoLista:: [Int] -> [Int]
cuadradoLista = map (^2) --x = x * x

cuadrado:: Int -> Int
cuadrado a = a ^ 2

main :: IO ()
main = do
    let resultado = cuadrado 2
    print ( cuadradoLista [1,2,3,4,5] )
    putStrLn $ "El cuadrado es: " ++ show resultado