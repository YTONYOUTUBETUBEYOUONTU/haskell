import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)

-- Definir el lexer
lexer = makeTokenParser emptyDef
parens = Text.Parsec.Token.parens lexer
integer = Text.Parsec.Token.integer lexer
float = Text.Parsec.Token.float lexer
reservedOp = Text.Parsec.Token.reservedOp lexer
whiteSpace = Text.Parsec.Token.whiteSpace lexer

-- Analizador de expresiones con operadores y paréntesis
exprParser :: Parser Double
exprParser = buildExpressionParser table factor
  where
    table = [ [Prefix (Text.Parsec.Token.reservedOp "-" >> return negate)]  -- Manejo de negativos
            , [Infix (Text.Parsec.Token.reservedOp "*" >> return (*)) AssocLeft,
               Infix (Text.Parsec.Token.reservedOp "/" >> return (/)) AssocLeft]
            , [Infix (Text.Parsec.Token.reservedOp "+" >> return (+)) AssocLeft,
               Infix (Text.Parsec.Token.reservedOp "-" >> return (-)) AssocLeft]
            ]
    factor = try Text.Parsec.Token.float <|> (fromIntegral <$> Text.Parsec.Token.integer) <|> Text.Parsec.Token.parens exprParser

-- Analiza la sintaxis de la expresión sin evaluarla
analizarSintaxis :: String -> Either String ()
analizarSintaxis input = case parse (Text.Parsec.Token.whiteSpace >> exprParser >> eof) "" input of
    Left err  -> Left $ "Error de sintaxis: " ++ show err
    Right _   -> Right ()

-- Evalúa la expresión si la sintaxis es correcta
evaluar :: String -> Either String Double
evaluar input = case parse exprParser "" input of
    Left err  -> Left $ "Error de evaluación: " ++ show err
    Right val -> Right val

-- Interacción con el usuario
main :: IO ()
main = do
    putStrLn "Ingrese una expresión aritmética en notación infija (ejemplo: '3 + 4 * 2'):"
    expr <- getLine
    case analizarSintaxis expr of
        Left err  -> putStrLn err
        Right _   -> case evaluar expr of
            Right val -> putStrLn $ "Resultado: " ++ show val
            Left err  -> putStrLn err
