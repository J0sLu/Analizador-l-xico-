module MDD where

import AFDmin
import Data.List (find)
import AFD

-- ------------------------------------------------------------
-- Definición de la Máquina Discriminadora Determinista (MDD)
-- ------------------------------------------------------------
data MDD = MDD
  { estadosMDD :: [[Estado]]
  , alfabetoMDD :: [Simbolo]
  , transicionesMDD :: [([Estado], Simbolo, [Estado])]
  , inicialMDD :: [Estado]
  , finalesMDD :: [[Estado]]
  } deriving (Show, Eq)

-- ------------------------------------------------------------
-- Conversión: AFDmin → MDD
-- ------------------------------------------------------------
afdminToMDD :: AFDmin -> MDD
afdminToMDD afdmin = MDD
  { estadosMDD = estadosAFDmin afdmin
  , alfabetoMDD = alfabetoAFDmin afdmin
  , transicionesMDD = transicionesAFDmin afdmin
  , inicialMDD = estadoInicialAFDmin afdmin
  , finalesMDD = estadosFinalesAFDmin afdmin
  }

-- ------------------------------------------------------------
-- Ejecución de la MDD con una cadena de entrada
-- ------------------------------------------------------------
ejecutarMDD :: MDD -> String -> Bool
ejecutarMDD mdd = recorrer (inicialMDD mdd)
  where
    recorrer estadoActual [] =
      estadoActual `elem` finalesMDD mdd

    recorrer estadoActual (c:cs) =
      case find (\(origen, simbolo, _) -> origen == estadoActual && simbolo == c)
                (transicionesMDD mdd) of
        Just (_, _, destino) -> recorrer destino cs
        Nothing              -> False