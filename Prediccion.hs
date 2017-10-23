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


pedirPrediccionCorrecta :: IO String
pedirPrediccionCorrecta = do
    putStrLn "He fallado! Cuál era la respuesta correcta?"
    getLine

pedirPregunta :: String -> IO String
pedirPregunta prediccionCorrecta = do 
    putStrLn $ "Que pregunta distingue a " ++ prediccionCorrecta 
                ++ " de las otras opciones?"
    getLine

pedirRespuesta :: String -> String -> IO String
pedirRespuesta pregunta prediccion = do 
    putStrLn $ "Cuál es la respuesta a \"" ++ pregunta ++ 
                "\" para " ++ prediccion ++ "?"
    getLine
------------------------------------------------------


predecir :: Oraculo -> IO Oraculo
predecir (Prediccion p)       = predecirPrediccion (Prediccion p)
predecir (Pregunta p ops)  = predecirPregunta (Pregunta p ops)


listaOpciones :: Opciones -> [String]
listaOpciones ops = P.map fst (toList ops)

showOpciones :: [String] -> String
showOpciones ops = P.foldl concatenar (head ops) (tail ops)
    where concatenar a b = a ++ " / " ++ b


insertar :: String -> Oraculo -> Oraculo -> Oraculo
insertar op prediccion (Pregunta p ops)  = Pregunta p (insert op prediccion ops)
insertar _ _ _= error ("El oraculo no es una pregunta")


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



predecirPrediccion :: Oraculo -> IO Oraculo
predecirPrediccion oraculo = do
    putStrLn $ "Prediccion: " ++ (prediccion oraculo)
    putStrLn $ showOpciones opcionesSiNo
    x <- obtenerRespuestaValida opcionesSiNo
    case x of
        "Si" -> return oraculo
        "No" -> prediccionFallida oraculo
    where opcionesSiNo = ["Si", "No"]

prediccionFallida :: Oraculo -> IO Oraculo
prediccionFallida oraculo = do
    prediccionCorrecta <- pedirPrediccionCorrecta
    pregunta <- (pedirPregunta prediccionCorrecta)
    opcionPrediccionCorrecta  <- (pedirRespuesta pregunta prediccionCorrecta)
    opcionPrediccionOraculo   <- (pedirRespuesta pregunta (prediccion oraculo))
    return ( ramificar  [opcionPrediccionOraculo, opcionPrediccionCorrecta]
                        [oraculo, crearOraculo prediccionCorrecta]
                        pregunta )
            

------------------------ Main -----------------

oraculoInicial = Pregunta "Es un lenguaje de programaci\243n?" (fromList [("No",Prediccion "HTML"),("Si",Pregunta "A qu\233 paradigma pertenece?" (fromList [("Funcional",Prediccion "Haskell"),("Imperativo",Pregunta "A qui\233n pertenece el lenguaje?" (fromList [("Microsoft",Prediccion "C#"),("Oracle",Prediccion "Java")])),("L\243gico",Prediccion "Prolog")]))])


predecirInfinito :: Oraculo -> IO Oraculo
predecirInfinito oraculo = do
    putStrLn "Quiere Imprimir el Oraculo? "
    imprimir <- getLine
    case imprimir of
        "Si" -> putStrLn $ show oraculo
        otherwise -> return ()
    nuevoOraculo <- (predecir oraculo)
    predecirInfinito nuevoOraculo

main = do
    predecirInfinito oraculoInicial
    putStrLn "Chao :)"