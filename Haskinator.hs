module Main(main) where
import Oraculo
import System.Exit

main::IO()
main = menu

menu::IO()
menu = do
        putStrLn "Introduzca una opción."
        putStrLn . unlines $ map showChoices choices
        choice <- getLine
        case validate choice of
            Just n  -> execChoice . read $ choice
            Nothing -> do 
                putStrLn "Opción incorrecta."
        menu
    where showChoices (i, s) = show i ++ ". " ++ s

validate :: String -> Maybe Int
validate s = isValid (reads s)
    where isValid [] = Nothing
          isValid ((n, _):_) 
                | (n < 1) || (n > length choices) = Nothing
                | otherwise = Just n


execChoice :: Int -> IO ()
execChoice 6 = exitSuccess
execChoice _ = opcion



choices :: [(Int, String)]
choices = zip [1.. ] [
        "Crear un oráculo nuevo",
        "Predecir",
        "Persistir",
        "Cargar",
        "Consultar pregunta crucial",
        "Salir"
    ]
 
 
opcion::IO()
opcion = do 
    putStrLn "opcion"