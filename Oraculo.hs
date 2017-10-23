module Oraculo where
import Data.Map as M

type Opciones = Map String Oraculo
data Oraculo = Pregunta String Opciones | Prediccion String 
                deriving(Show,Read)

crearOraculo :: String -> Oraculo
crearOraculo s = Prediccion s

prediccion :: Oraculo -> String
prediccion (Prediccion s) = s
prediccion _ = error "No es una predicción."

pregunta :: Oraculo -> String
pregunta (Pregunta s _) = s
pregunta _ = error "No es una pregunta."

opciones :: Oraculo -> Opciones
opciones (Pregunta _ opciones) = opciones
opciones _ = error "No es una pregunta."

respuesta :: Oraculo -> String -> Oraculo
respuesta (Pregunta _ opciones) key =
    case (M.lookup key opciones) of
        Just r -> r
        Nothing -> error "No es una respuesta válida."
respuesta _ _ = error "No es una pregunta."

ramificar :: [String] -> [Oraculo] -> String -> Oraculo
ramificar opciones oraculos pregunta =
    Pregunta pregunta (fromList (zip opciones oraculos))
