factorial:: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

main :: IO ()
main = do
    let resultado = factorial 5
    print (resultado)
    print (factorial 1)