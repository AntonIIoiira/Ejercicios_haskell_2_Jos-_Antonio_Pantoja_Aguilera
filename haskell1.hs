import Data.List (intercalate)
import Text.Printf (printf)

-- Función para calcular el seno, coseno, tangente, exponencial y logaritmo neperiano
calcular :: String -> Double -> [(Int, Double)]
calcular funcion valor = case funcion of
    "seno" -> [(x, sin (fromIntegral x)) | x <- [1..truncate valor]]
    "coseno" -> [(x, cos (fromIntegral x)) | x <- [1..truncate valor]]
    "tangente" -> [(x, tan (fromIntegral x)) | x <- [1..truncate valor]]
    "exponencial" -> [(x, exp (fromIntegral x)) | x <- [1..truncate valor]]
    "logaritmo" -> [(x, log (fromIntegral x)) | x <- [1..truncate valor]]
    _ -> error "Función no válida"

-- Función para imprimir la tabla
imprimirTabla :: [(Int, Double)] -> IO ()
imprimirTabla tabla = do
    putStrLn "Valor | Resultado"
    putStrLn "------+----------"
    mapM_ (\(x, y) -> printf "%5d | %f\n" x y) tabla

-- Función principal
main :: IO ()
main = do
    putStrLn "Ingrese la función a aplicar (seno, coseno, tangente, exponencial, logaritmo):"
    funcion <- getLine
    putStrLn "Ingrese el valor máximo:"
    valorStr <- getLine
    let valor = read valorStr :: Double
    let tabla = calcular funcion valor
    imprimirTabla tabla
