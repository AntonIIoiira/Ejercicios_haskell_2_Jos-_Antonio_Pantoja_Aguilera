import Data.Char (toUpper)

-- Función para obtener la calificación correspondiente a una nota
obtenerCalificacion :: Int -> String
obtenerCalificacion nota
    | nota >= 95 && nota <= 100 = "Excelente"
    | nota >= 85 && nota <= 94 = "Notable"
    | nota >= 75 && nota <= 84 = "Bueno"
    | nota >= 70 && nota <= 74 = "Suficiente"
    | otherwise = "Desempeño insuficiente"

-- Función para convertir las claves del diccionario a mayúsculas
convertirAMayusculas :: [(String, Int)] -> [(String, Int)]
convertirAMayusculas = map (\(asignatura, nota) -> (map toUpper asignatura, nota))

-- Función para filtrar las notas aprobadas y obtener el nuevo diccionario
filtrarNotasAprobadas :: [(String, Int)] -> [(String, String)]
filtrarNotasAprobadas = map (\(asignatura, nota) -> (asignatura, obtenerCalificacion nota)) . filter (\(_, nota) -> nota >= 70)

-- Función principal que toma un diccionario con las asignaturas y las notas y devuelve otro diccionario con las asignaturas en mayúsculas y las calificaciones correspondientes a las notas aprobadas
procesarNotas :: [(String, Int)] -> [(String, String)]
procesarNotas = filtrarNotasAprobadas . convertirAMayusculas

-- Ejemplo de uso
main :: IO ()
main = do
    let diccionarioNotas = [("Matematicas", 85), ("Ciencias", 65), ("Historia", 92), ("Ingles", 75)]
    let nuevoDiccionario = procesarNotas diccionarioNotas
    putStrLn "Diccionario de notas original:"
    print diccionarioNotas
    putStrLn "Nuevo diccionario con las asignaturas en mayúsculas y las calificaciones correspondientes a las notas aprobadas:"
    print nuevoDiccionario

