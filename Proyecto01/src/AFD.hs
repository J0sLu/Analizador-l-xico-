module AFD where

import AFN
import Data.List (nub, sort)
import qualified Data.Set as Set
-------------------------
-- Definición de tipos
-------------------------
type Estado = String    
type Simbolo = Char     

-- Estructura de un AFD (Automáta Finito Determinista)
data AFD = AFD
  { estadosAFD :: [[Estado]]          
  , alfabetoAFD :: [Simbolo]          
  , transicionesAFD :: [([Estado], Simbolo, [Estado])] 
  , estadoInicialAFD :: [Estado]      
  , estadosFinalesAFD :: [[Estado]]  
  } deriving (Show)

-- Función que calcula el movimiento de un conjunto de estados en el AFN
mueveConjunto :: AFN -> [Estado] -> Simbolo -> [Estado]
mueveConjunto afn estados simbolo =
  nub . concat $  -- elimina duplicados
    [ destino
    | (origen, s, destino) <- transicionesAFN afn  
    , s == simbolo                                 
    , origen `elem` estados                         
    ]

-- Conversión de un AFN a un AFD usando el algoritmo de subconjuntos
afnToAfd :: AFN -> AFD
afnToAfd afn =
  let
    -- Estado inicial del AFD: conjunto con el estado inicial del AFN
    inicial = sort [estadoInicialAFN afn]

    -- Función recursiva que construye todos los estados y transiciones del AFD
    construir estadosVisitados [] trans = (estadosVisitados, trans) 
    construir estadosVisitados (actual:pendientes) trans =
      let nuevasTrans =
            [ (actual, a, sort (mueveConjunto afn actual a))  
            | a <- alfabetoAFN afn
            ]
          -- identificamos los nuevos estados que aún no hemos visitado
          nuevosEstados =
            [ destino
            | (_, _, destino) <- nuevasTrans
            , not (null destino)  -- descartamos conjuntos vacíos
            , destino `notElem` (actual : estadosVisitados ++ pendientes)
            ]
      in construir (nub (actual : estadosVisitados))    
                   (pendientes ++ nuevosEstados)       
                   (nub (trans ++ nuevasTrans))        

    -- Construimos los estados deterministas y sus transiciones
    (estadosDeterministas, transicionesDet) =
      construir [] [inicial] []

    -- Determinamos los estados finales del AFD
    finales =
      [ e | e <- estadosDeterministas
          , any (`elem` estadosFinalesAFN afn) e  
      ]
  in
    AFD
      { estadosAFD = estadosDeterministas
      , alfabetoAFD = alfabetoAFN afn
      , transicionesAFD = transicionesDet
      , estadoInicialAFD = inicial
      , estadosFinalesAFD = finales
      }