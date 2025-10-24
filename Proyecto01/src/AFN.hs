module AFN where
import AFNe
import Data.List (nub)

data AFN = AFN
  { estadosAFN :: [Estado]
  , alfabetoAFN :: [Simbolo]
  , transicionesAFN :: [(Estado, Simbolo, [Estado])]
  , estadoInicialAFN :: Estado
  , estadosFinalesAFN :: [Estado]
  } deriving (Show, Eq)


-- Funciones auxiliares: movimiento y cierre-ε
mueve :: AFNe -> Estado -> Maybe Simbolo -> [Estado]
mueve afn e s =
  concat [ es | (origen, simb, es) <- transiciones afn
              , origen == e
              , simb == s ]

epsilonCierre :: AFNe -> [Estado] -> [Estado]
epsilonCierre afn estadosIniciales =
  let nuevos = nub (estadosIniciales ++ concat [ mueve afn e Nothing | e <- estadosIniciales ])
  in if length nuevos == length estadosIniciales
       then nuevos
       else epsilonCierre afn nuevos

-- Eliminación de transiciones epsilon: AFNe → AFN
afneToAfn :: AFNe -> AFN
afneToAfn afne =
  let
    -- Calcular el ε-cierre de cada estado
    cierres = [(e, epsilonCierre afne [e]) | e <- estados afne]

    -- Generar nuevas transiciones para cada símbolo del alfabeto
    nuevasTransiciones =
      [ (p, a, nub . concat $
          [ epsilonCierre afne (mueve afne q (Just a))
          | q <- cierreP ])
      | (p, cierreP) <- cierres
      , a <- alfabeto afne
      ]

    -- Filtrar las transiciones sin destino
    transicionesFiltradas =
      [ (p,a,es) | (p,a,es) <- nuevasTransiciones, not (null es) ]

    -- Calcular nuevos estados finales
    nuevosFinales =
      [ p | (p, cierreP) <- cierres
          , any (`elem` estadosFinales afne) cierreP
      ]
  in
    AFN
      { estadosAFN = estados afne
      , alfabetoAFN = alfabeto afne
      , transicionesAFN = transicionesFiltradas
      , estadoInicialAFN = estadoInicial afne
      , estadosFinalesAFN = nub nuevosFinales
      }