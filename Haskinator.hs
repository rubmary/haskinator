{-
Módulo  : Haskinator
Autores : Rubmary Rojas 13-11264, Carlos Infante 13-10681

Programa principal Haskinator. Presenta un menú de opciones
al usuario. Le permite realizar predicciones sobre un Oraculo
existente y modificarlo, o empezar un Oraculo desde una primera
Pediccion.
-}
module Main(main) where
import Oraculo
import System.Exit
import System.IO
import Data.Map as M
import Prelude as P

-- Representa la respuesta para el ancestro común más bajo.
-- Si es Lca, contiene tres cadenas de caracteres con la respuesta.
-- Si es First, solo existe la primera opción.
-- Si es Second, solo existe la segunda opción.
-- Si es None, no existe ninguna opción.
data Lca =  Lca String String String | First | Second | None

-- Muestra al usuario una serie de opciones numeradas, mantiene un Oraculo
-- que se usará para las predicciones, y avisa al usuario cuando
-- introduce una opción incorrecta.
menu :: Oraculo -> IO()
menu oraculoActual = do
        putStrLn "Introduzca una opción."
        putStrLn . unlines $ P.map showChoices choices
        choice <- getLine
        oraculoNuevo <- case read choice of
            1 -> crearNuevo
            2 -> predecir oraculoActual
            3 -> persistir oraculoActual
            4 -> cargar
            5 -> obtenerPreguntaCritica oraculoActual
            6 -> exitSuccess
            _ -> do
                putStrLn "Opción incorrecta."
                return oraculoActual
        menu oraculoNuevo
    where showChoices (i, s) = show i ++ ". " ++ s

-- Opciones numeradas para el menú.
choices :: [(Int, String)]
choices = zip [1.. ] [
        "Crear un oráculo nuevo",
        "Predecir",
        "Persistir",
        "Cargar",
        "Consultar pregunta crucial",
        "Salir"
    ]

-- Pide al usuario una cadena de caracteres y crea un
-- nuevo Oraculo incluyendo solo una Prediccion.
crearNuevo :: IO Oraculo
crearNuevo = do
    putStrLn "Introduzca una predicción."
    pred <- getLine
    return $ crearOraculo pred    

-- Pide al usuario que introduzca la ruta de un archivo
-- y crea un Oraculo con su contenido.
cargar :: IO Oraculo
cargar = do
        putStrLn "Introduzca el archivo a leer."
        archivo <- getLine
        s <- readFile archivo
        return $ oraculo s
    where
        oraculo s = read s :: Oraculo


-- Recibe un Oraculo, pide al usuario que introduzca un nombre
-- de archivo, y guarda el Oraculo en el archivo.
persistir :: Oraculo -> IO Oraculo
persistir oraculo = do
    putStrLn "Introduzca el archivo para guardar el oráculo."
    archivo <- getLine
    writeFile archivo (show oraculo)
    return $ oraculo

--------------------------------  Pregunta crucial -------------------------------------

-- Recibe dos cadenas de caracteres que representan predicciones
-- y un Oraculo, y encuentra su ancestro común más bajo en el Oraculo.
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

-- Recibe dos cadenas de caracteres que representan predicciones
-- y un Oraculo, aplica lca y transforma la respuesta del tipo Lca
-- a una lista de cadenas de caracteres con la respuesta o un error.
preguntaCritica :: String -> String -> Oraculo -> [String]
preguntaCritica s1 s2 oraculo =
    case (lca s1 s2 oraculo) of
        Lca p op1 op2   -> [p, op1, op2]
        First           -> ["La segunda opción no existe."]
        Second          -> ["La primera opción no existe."]
        otherwise       -> ["Ninguna de las opciones existe."]

-- Recibe dos predicciones del usuario, y busca sobre el Oraculo
-- actual la pregunta crucial que las separa. Simplemente devuelve
-- una advertencia en caso de que alguna predicción no exista.
obtenerPreguntaCritica :: Oraculo -> IO Oraculo
obtenerPreguntaCritica oraculo = do
    putStrLn "Escribir primera prediccion"
    s1 <- getLine
    putStrLn "Escribir segunda prediccion"
    s2 <- getLine
    mostrarPreguntaCritica (preguntaCritica s1 s2 oraculo) s1 s2
    return oraculo

-- Muestra la pregunta crítica al usario, y las opciones que llevan
-- a cada Prediccion.
mostrarPreguntaCritica :: [String] -> String -> String -> IO()
mostrarPreguntaCritica (p:op1:op2:_) s1 s2 = do
    putStrLn ("La pregunta crucial es " ++ p)
    putStrLn ("La respuesta para " ++ s1 ++ " es " ++ op1)
    putStrLn ("La respuesta para " ++ s2 ++ " es " ++ op2)
mostrarPreguntaCritica (err:_) _ _ = putStrLn err

--------------------------------  Predicciones ---------------------------------------

-- Inicia o continúa una secuencia de predicción.
predecir :: Oraculo -> IO Oraculo
predecir (Prediccion p)    = predecirPrediccion (Prediccion p)
predecir (Pregunta p ops)  = predecirPregunta (Pregunta p ops)

-- Se plantea al usuario una pregunta y sus opciones. Si el usuario responde
-- "ninguna", se le pide al usuario la opción que esperaba y su respuesta.
-- De otra forma, la predicción continúa hacia el sub-oráculo necesario.
predecirPregunta :: Oraculo -> IO Oraculo
predecirPregunta oraculo = do 
    putStrLn $ pregunta oraculo
    putStrLn $ showOpciones $ (listaOpciones $ opciones oraculo)
    usuario <- obtenerRespuestaValida ("ninguna": (listaOpciones (opciones oraculo)))
    case usuario of
        "ninguna" -> do
            prediccionCorrecta <- pedirPrediccionCorrecta
            opcion <- pedirRespuesta (pregunta oraculo) prediccionCorrecta
            return (insertar opcion (crearOraculo prediccionCorrecta) oraculo)
        opcion -> do
            subOraculo <- predecir (respuesta oraculo opcion)
            return (insertar opcion subOraculo oraculo)


-- Se plantea al usuario una predicción, y este debe decidir si es correcta.
-- En caso negativo, se le pide la opción correcta y una pregunta que permita discernirla.
predecirPrediccion :: Oraculo -> IO Oraculo
predecirPrediccion oraculo = do
    putStrLn $ "Prediccion: " ++ (prediccion oraculo)
    putStrLn $ showOpciones opcionesSiNo
    x <- obtenerRespuestaValida opcionesSiNo
    case x of
        "Si" -> return oraculo
        "No" -> prediccionFallida oraculo
    where opcionesSiNo = ["Si", "No"]

-- Se le pide el usuario la predicción correcta y la pregunta para llegar a ella.
-- Se modifica el oráculo para incluir la nueva información.
prediccionFallida :: Oraculo -> IO Oraculo
prediccionFallida oraculo = do
    prediccionCorrecta <- pedirPrediccionCorrecta
    pregunta <- (pedirPregunta prediccionCorrecta)
    opcionPrediccionCorrecta  <- (pedirRespuesta pregunta prediccionCorrecta)
    opcionPrediccionOraculo   <- (pedirRespuesta pregunta (prediccion oraculo))
    return ( ramificar  [opcionPrediccionOraculo, opcionPrediccionCorrecta]
                        [oraculo, crearOraculo prediccionCorrecta]
                        pregunta )

-- Inserta una nueva opción para una pregunta.
insertar :: String -> Oraculo -> Oraculo -> Oraculo
insertar op prediccion (Pregunta p ops)  = Pregunta p (insert op prediccion ops)
insertar _ _ _= error ("El oraculo no es una pregunta")

-------------------------  Interacciones con el usuario ------------------------

-- Verifica que la respuesta del usuario a una pregunta sea válida.
-- Para que sea válida, la respuesta debe formar parte de las Opciones.
obtenerRespuestaValida :: [String] -> IO String
obtenerRespuestaValida opciones = do
    respuesta <- getLine
    case (elem respuesta opciones) of
        True  -> return respuesta
        False -> do
            putStrLn "Opcion inválida, intente de nuevo!"
            obtenerRespuestaValida opciones

-- Pide al usuario la opción correcta.
pedirPrediccionCorrecta :: IO String
pedirPrediccionCorrecta = do
    putStrLn "He fallado! Cuál era la respuesta correcta?"
    getLine

-- Pide al usuario la pregunta que distinga la opción correcta.
pedirPregunta :: String -> IO String
pedirPregunta prediccionCorrecta = do
    putStrLn $ "Que pregunta distingue a " ++ prediccionCorrecta
                ++ " de las otras opciones?"
    getLine

-- Pide la respuesta a la pregunta para una predicción.
pedirRespuesta :: String -> String -> IO String
pedirRespuesta pregunta prediccion = do
    putStrLn $ "Cuál es la respuesta a \"" ++ pregunta ++
                "\" para " ++ prediccion ++ "?"
    obtenerOpcionValida

-- Verifica que la opción introducida por el usuario sea correcta.
-- "ninguna" no es una opción válida pues está reservada para que
-- el usuario agregue una opción adicional.
obtenerOpcionValida :: IO String
obtenerOpcionValida = do
    opcion <- getLine
    case opcion of
        "ninguna" -> do
            putStrLn "\"ninguna\" no puede ser respuesta. Intente de nuevo."
            obtenerOpcionValida
        _ -> do
            return opcion

-- Recibe Opciones y devuelve la lista de cadenas de caracteres
-- asociada con ellas.
listaOpciones :: Opciones -> [String]
listaOpciones ops = P.map fst (toList ops)

-- Muestra la lista de cadena de caracteres asociada con la lista de opciones.
showOpciones :: [String] -> String
showOpciones ops = P.foldl concatenar (head ops) (tail ops)
    where concatenar a b = a ++ " / " ++ b



-- Ejecuta el menú con un primer oráculo vacío.
main::IO()
main = menu $ crearOraculo ""