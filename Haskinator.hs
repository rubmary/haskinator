module Main(main) where
import Oraculo
import System.Exit
import System.IO

main::IO()
main = menu $ crearOraculo ""

menu :: Oraculo -> IO()
menu oraculoActual = do
        putStrLn "Introduzca una opción."
        putStrLn . unlines $ map showChoices choices
        choice <- getLine
        oraculoNuevo <- case read choice of
            1 -> crearNuevo
            3 -> persistir oraculoActual
            4 -> cargar
            6 -> exitSuccess
            _ -> do
                putStrLn "Opción incorrecta."
                return oraculoActual
        putStrLn $ "Oráculo actual: " ++ (show oraculoActual)
        putStrLn $ "Oráculo nuevo: " ++ (show oraculoNuevo)
        menu oraculoNuevo
    where showChoices (i, s) = show i ++ ". " ++ s



choices :: [(Int, String)]
choices = zip [1.. ] [
        "Crear un oráculo nuevo",
        "Predecir",
        "Persistir",
        "Cargar",
        "Consultar pregunta crucial",
        "Salir"
    ]

crearNuevo :: IO Oraculo
crearNuevo = do
    putStrLn "Introduzca una predicción."
    pred <- getLine
    return $ crearOraculo pred    

cargar :: IO Oraculo
cargar = do
        putStrLn "Introduzca el archivo a leer."
        archivo <- getLine
        s <- readFile archivo
        return $ oraculo s
    where
        oraculo s = read s :: Oraculo

persistir :: Oraculo -> IO Oraculo
persistir oraculo = do
    putStrLn "Introduzca el archivo para guardar el oráculo."
    archivo <- getLine
    writeFile archivo (show oraculo)
    return $ oraculo