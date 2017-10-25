{-
Módulo  : Oraculo
Autores : Rubmary Rojas 13-11264, Carlos Infante 13-10681

Módulo incluyendo las definiciones para los tipos de datos
y sus funciones de creación, acceso y modificación.
-}
module Oraculo where
import Data.Map as M

-- Una Opcion es un diccionario que contiene informacion de la opción escogida
-- en la pregunta anterior y cómo continuar con la predicción.
type Opciones = Map String Oraculo

-- Un Oraculo puede ser una Pregunta o una Prediccion.
-- Una Prediccion contiene una cadena de caracteres.
-- Una Pregunta contiene una cadena de caracteres y una serie de
-- opciones como respuesta.
data Oraculo = Pregunta String Opciones | Prediccion String 
                deriving(Show,Read)

-- Recibe una cadena de caracteres y devuelve un Oraculo como Prediccion.
crearOraculo :: String -> Oraculo
crearOraculo s = Prediccion s

-- Recibe un Oraculo y devuelve su cadena de caracteres si es una Prediccion.
-- Arroja error si el Oraculo no es una Prediccion.
prediccion :: Oraculo -> String
prediccion (Prediccion s) = s
prediccion _ = error "No es una predicción."

-- Recibe un Oraculo y devuelve su cadena de caracteres si es una Pregunta.
-- Arroja error si el Oraculo no es una Pregunta.
pregunta :: Oraculo -> String
pregunta (Pregunta s _) = s
pregunta _ = error "No es una pregunta."

-- Recibe un Oraculo y devuelve sus Opciones si es una Pregunta.
-- Arroja error si el Oraculo no es una Pregunta.
opciones :: Oraculo -> Opciones
opciones (Pregunta _ opciones) = opciones
opciones _ = error "No es una pregunta."

-- Recibe un Oraculo y una cadena de caracteres y devuelve el Oraculo
-- asociado a la cadena, si es una Pregunta.
-- Arroja error si el Oraculo no es una Pregunta.
respuesta :: Oraculo -> String -> Oraculo
respuesta (Pregunta _ opciones) key =
    case (M.lookup key opciones) of
        Just r -> r
        Nothing -> error "No es una respuesta válida."
respuesta _ _ = error "No es una pregunta."

-- Recibe una lista de cadenas de caracteres, una lista de Oraculos,
-- y una cadena de caracteres. Devuelve un Oraculo de tipo Pregunta
-- usando la cadena como pregunta y las listas como Opciones.
ramificar :: [String] -> [Oraculo] -> String -> Oraculo
ramificar opciones oraculos pregunta =
    Pregunta pregunta (fromList (zip opciones oraculos))
