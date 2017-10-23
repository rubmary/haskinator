module Main(main) where
import Oraculo
import Data.Map as M
import Prelude as P


-----------  Interacciones con el usuario --------------
obtenerRespuestaValida :: [String] -> IO String
obtenerRespuestaValida opciones = do
    respuesta <- getLine
    case (elem respuesta opciones) of
        True  -> return respuesta
        False -> do
            putStrLn "Opcion invalida, intente de nuevo!"
            obtenerRespuestaValida opciones


pedirRepuestacorrecta :: IO String
pedirRepuestacorrecta = do
    putStrLn "He fallado! Cuál era la respuesta correcta?"
    getLine

pedirPregunta :: String -> IO String
pedirPregunta opcion = do 
    putStrLn $ "Que pregunta distingue a " ++ opcion ++ " de las otras opciones?"
    getLine

pedirRespuesta :: String -> String -> IO String
pedirRespuesta pregunta respuesta = do 
    putStrLn $ "Cuál es la respuesta a \"" ++ pregunta ++ 
                "\" para " ++ respuesta ++ "?"
    getLine

------------------------------------------------------


predecir :: Oraculo -> IO Oraculo
predecir oraculo = do
    return oraculo

showOpciones :: Opciones -> String
showOpciones ops = P.foldl concatenar (head preguntas) (tail preguntas)
    where   preguntas = P.map fst (toList ops)
            concatenar a b = a ++ " / " ++ b


insertar :: String -> String -> Oraculo -> Oraculo
insertar op res (Pregunta p ops)  = Pregunta p (insert op (crearOraculo res) ops)
insertar _ _ _= error ("El oraculo no es una pregunta")


predecirPregunta :: Oraculo -> IO Oraculo
predecirPregunta oraculo = do 
    putStrLn $ pregunta oraculo
    putStrLn $ showOpciones (opciones oraculo)
    respuesta <- getLine
    case respuesta of
        "ninguna" -> do
            respuesta <- pedirRepuestacorrecta
            putStrLn (pregunta oraculo)
            opcion <- getLine
            return (insertar opcion respuesta oraculo)
        s -> predecir ((opciones oraculo) ! s)


prediccionFallida :: Oraculo -> IO Oraculo
prediccionFallida oraculo = do
    respuesta <- pedirRepuestacorrecta
    pregunta <- (pedirPregunta respuesta)
    opcionPrediccion <- (pedirRespuesta pregunta (prediccion oraculo))
    opcionRespuesta  <- (pedirRespuesta pregunta respuesta)
    return ( ramificar  [opcionPrediccion, opcionRespuesta]
                        [oraculo, crearOraculo opcionPrediccion]
                        pregunta )

predecirPrediccion :: Oraculo -> IO Oraculo
predecirPrediccion oraculo = do
    putStrLn $ "Prediccion" ++ (prediccion oraculo)
    x <- getLine
    case x of
        "si" -> return oraculo
        "no" -> prediccionFallida oraculo
            

------------------------ Main -----------------
main = do
    let z = (singleton 1 'a')
    putStrLn "hola :)"