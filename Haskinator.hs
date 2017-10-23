module Main(main) where
import Oraculo
import System.Exit
import System.IO
import Data.Map as M
import Prelude as P

data Lca =  Lca String String String | First | Second | None

menu :: Oraculo -> IO()
menu oraculoActual = do
        putStrLn "Introduzca una opción."
        putStrLn . unlines $ P.map showChoices choices
        choice <- getLine
        oraculoNuevo <- case read choice of
            1 -> crearNuevo
            3 -> persistir oraculoActual
            4 -> cargar
            5 -> obtenerPreguntaCritica oraculoActual
            6 -> exitSuccess
            _ -> do
                putStrLn "Opción incorrecta."
                return oraculoActual
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

lca :: String -> String -> Oraculo -> Lca
lca s1 s2 (Prediccion prediccion)
    | prediccion == s1  = First
    | prediccion == s2  = Second
    | otherwise         = None
lca s1 s2 oraculo = case s1 == s2 of
    True -> error("Las predicciones no pueden ser iguales")
    otherwise ->
        case P.filter noNone listaLcas of
            []          -> None
            [(x, op)]   -> x
            [(First,  op1), (Second, op2)]  -> Lca (pregunta oraculo) op1 op2
            [(Second, op2), (First,  op1)]  -> Lca (pregunta oraculo) op1 op2
        where
            lcaAux (a, b) = (lca s1 s2 b, a)
            listaLcas = P.map lcaAux (toList (opciones oraculo))
            noNone (None, _) = False
            noNone _ = True

preguntaCritica :: String -> String -> Oraculo -> [String]
preguntaCritica s1 s2 oraculo =
    case (lca s1 s2 oraculo) of
        Lca p op1 op2   -> [p, op1, op2]
        First           -> ["La segunda opción no existe."]
        Second          -> ["La primera opción no existe."]
        otherwise       -> ["Ninguna de las opciones existe."]

obtenerPreguntaCritica :: Oraculo -> IO Oraculo
obtenerPreguntaCritica oraculo = do
    putStrLn "Escribir primera prediccion"
    s1 <- getLine
    putStrLn "Escribir segunda prediccion"
    s2 <- getLine
    mostrarPreguntaCritica (preguntaCritica s1 s2 oraculo) s1 s2
    return oraculo

mostrarPreguntaCritica :: [String] -> String -> String -> IO()
mostrarPreguntaCritica (p:op1:op2:_) s1 s2 = do
    putStrLn ("La pregunta crucial es " ++ p)
    putStrLn ("La respuesta para " ++ s1 ++ " es " ++ op1)
    putStrLn ("La respuesta para " ++ s2 ++ " es " ++ op2)
mostrarPreguntaCritica (err:_) _ _ = putStrLn err

main::IO()
main = menu $ crearOraculo ""