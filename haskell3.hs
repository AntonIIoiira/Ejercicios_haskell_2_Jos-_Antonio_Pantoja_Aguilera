-- Función para obtener la calificación correspondiente a una nota
obtenerCalificacion :: Int -> String
obtenerCalificacion nota
    | nota >= 95 && nota <= 100 = "Excelente"
    | nota >= 85 && nota <= 94 = "Notable"
    | nota >= 75 && nota <= 84 = "Bueno"
    | nota >= 70 && nota <= 74 = "Suficiente"
    | otherwise = "Desempeño insuficiente"

-- Función para obtener la lista de calificaciones correspondientes a una lista de notas
obtenerCalificaciones :: [Int] -> [String]
obtenerCalificaciones = map obtenerCalificacion

-- Ejemplo de uso
main :: IO ()
main = do
    let notas = [65, 80, 90, 70, 100]
    let calificaciones = obtenerCalificaciones notas
    putStrLn "Lista de notas:"
    print notas
    putStrLn "Lista de calificaciones correspondientes:"
    print calificaciones
