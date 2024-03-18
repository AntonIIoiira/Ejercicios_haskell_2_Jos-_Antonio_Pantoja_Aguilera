-- Función que recibe otra función booleana y una lista, y devuelve otra lista con los elementos que devuelvan True
filtrar :: (a -> Bool) -> [a] -> [a]
filtrar _ [] = []  
filtrar f (x:xs)
    | f x       = x : filtrar f xs 
    | otherwise = filtrar f xs 


main :: IO ()
main = do
    let lista = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    let funcionBooleana = even -- Función booleana para verificar si un número es par
    let listaFiltrada = filtrar funcionBooleana lista
    putStrLn $ "Lista original: " ++ show lista
    putStrLn $ "Lista filtrada: " ++ show listaFiltrada
