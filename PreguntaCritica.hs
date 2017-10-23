module Main(main) where
import Oraculo
import Data.Map as M
import Prelude as P


data Lca =  Lca String String String | First | Second | None

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
        First           -> ["1"]
        Second          -> ["2"]
        otherwise       -> []

------------------------------------- PRUEBAS ------------------------------

oraculoInicial = Pregunta "Es un lenguaje de programaci\243n?" (fromList [("No",Prediccion "HTML"),("Si",Pregunta "A qu\233 paradigma pertenece?" (fromList [("Funcional",Prediccion "Haskell"),("Imperativo",Pregunta "A qui\233n pertenece el lenguaje?" (fromList [("Microsoft",Prediccion "C#"),("Oracle",Prediccion "Java")])),("L\243gico",Prediccion "Prolog")]))])
main = do
    putStrLn "Escribir primera prediccion"
    s1 <- getLine
    putStrLn "Escribir segunda prediccion"
    s2 <- getLine
    putStrLn $ show (preguntaCritica s1 s2 oraculoInicial)
    main
