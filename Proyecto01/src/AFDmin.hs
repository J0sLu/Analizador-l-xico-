 module AFDmin where

import AFD
import Data.List (nub, sort)

-- ------------------------------------------------------------
-- Definición del tipo de datos para el AFD minimizado
-- ------------------------------------------------------------
data AFDmin = AFDmin
  { estadosAFDmin :: [[Estado]]                      
  , alfabetoAFDmin :: [Simbolo]                     
  , transicionesAFDmin :: [([Estado], Simbolo, [Estado])]
  , estadoInicialAFDmin :: [Estado]                  
  , estadosFinalesAFDmin :: [[Estado]]               
  } deriving (Show, Eq)

-- ------------------------------------------------------------
-- Minimización de un AFD
-- ------------------------------------------------------------
afdMinimizar :: AFD -> AFDmin
afdMinimizar afd =
  let
    -- Partición inicial: finales y no finales
    finales = estadosFinalesAFD afd
    noFinales = [e | e <- estadosAFD afd, e `notElem` finales]
    particionesIniciales = [finales, noFinales]

    -- Refinar particiones hasta que no cambien
    refinar :: [[[Estado]]] -> [[[Estado]]]
    refinar grupos =
      let nuevos = concatMap (dividir grupos) grupos
      in if sort nuevos == sort grupos
         then grupos
         else refinar nuevos

    -- Dividir un grupo según las clases destino
    dividir :: [[[Estado]]] -> [[Estado]] -> [[[Estado]]]
    dividir grupos grupo =
      let clases = nub (map (clase grupos) grupo)
      in [ [e | e <- grupo, clase grupos e == c] | c <- clases ]

    -- Determinar la clase de un estado según sus transiciones
    clase :: [[[Estado]]] -> [Estado] -> [Int]
    clase grupos estado =
      [ case [i | (i, g) <- zip [1..] grupos
                , any (`elem` g) (buscarDestino estado a)] of
          [] -> 0      -- Maneja el caso sin destino para evitar head []
          xs -> head xs
      | a <- alfabetoAFD afd
      ]

    -- Buscar los destinos de un conjunto de estados con un símbolo
    buscarDestino :: [Estado] -> Simbolo -> [[Estado]]
    buscarDestino es a =
      [ destino
      | (origen, simbolo, destino) <- transicionesAFD afd
      , any (`elem` origen) es
      , simbolo == a
      ]

    -- Aplicar refinamiento 
    particionesFinales = refinar particionesIniciales

    -- Asociar cada estado con su clase final
    buscarClase :: [Estado] -> [Estado]
    buscarClase e = head [g | g <- concat particionesFinales, any (`elem` g) e]

    -- Construir transiciones del AFDmin
    nuevasTransiciones =
      nub [ (buscarClase origen, simbolo, buscarClase destino)
          | (origen, simbolo, destino) <- transicionesAFD afd
          , not (null destino)
          ]

    -- Determinar estado inicial y finales del AFDmin
    inicial = buscarClase (estadoInicialAFD afd)
    nuevosFinales =
      [ g
      | g <- concat particionesFinales
      , any (any (`elem` g)) finales
      ]

  in
    AFDmin
      { estadosAFDmin = concat particionesFinales
      , alfabetoAFDmin = alfabetoAFD afd
      , transicionesAFDmin = nuevasTransiciones
      , estadoInicialAFDmin = inicial
      , estadosFinalesAFDmin = nuevosFinales
      }