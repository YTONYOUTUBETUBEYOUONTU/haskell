import Text.Read (readMaybe)
import Data.Maybe(fromMaybe)

evaluar :: String -> Maybe Double 
evaluar expr = case words expr of
    [a, op, b] -> do
        x <- readMaybe a
        y <- readMaybe b
        case op of
            "+" -> Just (x + y)
            "-" -> Just (x - y)
            "*" -> Just (x * y)
            "/" -> if y /= 0 then Just (x / y) else Nothing
            _ -> Nothing
    _ -> Nothing
main :: IO ()
main = do
    putStrLn "Ingrese una expresion aritmetica: "
    expr <- getLine
    case evaluar expr of
        Just val -> putStrLn $ "Resultado: " ++ show val
        Nothing -> putStrLn "Expresion invalida"