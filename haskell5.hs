data Inmueble = Inmueble {
    año :: Int,
    metros :: Int,
    habitaciones :: Int,
    garaje :: Bool,
    zona :: Char
} deriving (Show)

-- Función para calcular el precio de un inmueble en función de la zona y la antigüedad
calcularPrecio :: Inmueble -> Float
calcularPrecio inmueble =
    let precioBase = fromIntegral (metros inmueble * 1000 + habitaciones inmueble * 5000 + if garaje inmueble then 15000 else 0)
        antiguedad = 2024 - año inmueble  -- Suponiendo que el año actual es 2024
        factorZona = if zona inmueble == 'A' then 1 else 1.5
    in precioBase * (1 - fromIntegral antiguedad / 100) * factorZona

-- Función para filtrar los inmuebles según un presupuesto dado
buscarInmuebles :: [Inmueble] -> Float -> [Inmueble]
buscarInmuebles inmuebles presupuesto =
    filter (\inmueble -> calcularPrecio inmueble <= presupuesto) inmuebles

-- Ejemplo de uso
main :: IO ()
main = do
    let listaInmuebles = [
            Inmueble { año = 2000, metros = 100, habitaciones = 3, garaje = True, zona = 'A' },
            Inmueble { año = 2012, metros = 60, habitaciones = 2, garaje = True, zona = 'B' },
            Inmueble { año = 1980, metros = 120, habitaciones = 4, garaje = False, zona = 'A' },
            Inmueble { año = 2005, metros = 75, habitaciones = 3, garaje = True, zona = 'B' },
            Inmueble { año = 2015, metros = 90, habitaciones = 2, garaje = False, zona = 'A' }
            ]
    let presupuesto = 100000  -- Presupuesto dado
    let inmueblesFiltrados = buscarInmuebles listaInmuebles presupuesto
    putStrLn "Inmuebles filtrados:"
    print inmueblesFiltrados
