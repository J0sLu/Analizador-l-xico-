module AFNe where
import ER 

-- ------------------------------------------------------------
-- Tipos base
-- ------------------------------------------------------------

type Estado = String
type Simbolo = Char

-- Transición: (origen, símbolo (Nothing = ε), destinos)
type Transicion = (Estado, Maybe Simbolo, [Estado])

-- Estructura del autómata AFN con transiciones épsilon
data AFNe = AFNe
  { estados :: [Estado] -- Conjunto de estados
  , alfabeto :: [Simbolo] -- Símbolos del alfabeto
  , transiciones :: [Transicion] -- Lista de transiciones
  , estadoInicial :: Estado -- Estado inicial
  , estadosFinales :: [Estado] -- Lista de estados finales
  } deriving (Show, Eq)

-- Función principal: ER → AFNe
erAafne :: ER -> AFNe
erAafne er = fst (construirAFNe er 0)

-- Función auxiliar: genera nombres de estados
nuevoEstado :: Int -> Estado
nuevoEstado n = "q" ++ show n

-- Construcción recursiva del AFNe 
construirAFNe :: ER -> Int -> (AFNe, Int)

-- Caso: Conjunto vacío
construirAFNe Vacio n =
  let q0 = nuevoEstado n
      qf = nuevoEstado (n + 1)
   in (AFNe [q0, qf] [] [] q0 [qf], n + 2)

-- Caso: Epsilon
construirAFNe Epsilon n =
  let q0 = nuevoEstado n
      qf = nuevoEstado (n + 1)
      trans = [(q0, Nothing, [qf])]
   in (AFNe [q0, qf] [] trans q0 [qf], n + 2)

-- Caso: Símbolo individual
construirAFNe (Simbolo c) n =
  let q0 = nuevoEstado n
      qf = nuevoEstado (n + 1)
      trans = [(q0, Just c, [qf])]
   in (AFNe [q0, qf] [c] trans q0 [qf], n + 2)

-- Caso: Unión (a + b)
construirAFNe (Union e1 e2) n =
  let (a1, n1) = construirAFNe e1 n
      (a2, n2) = construirAFNe e2 n1
      q0 = nuevoEstado n2
      qf = nuevoEstado (n2 + 1)
      trans =
        [(q0, Nothing, [estadoInicial a1, estadoInicial a2])] ++
        transiciones a1 ++ transiciones a2 ++
        [(f, Nothing, [qf]) | f <- estadosFinales a1 ++ estadosFinales a2]
      sigma = alfabeto a1 ++ alfabeto a2
      qs = q0 : qf : (estados a1 ++ estados a2)
   in (AFNe qs sigma trans q0 [qf], n2 + 2)

-- Caso: Concatenación (a . b)
construirAFNe (Concatenacion e1 e2) n =
  let (a1, n1) = construirAFNe e1 n
      (a2, n2) = construirAFNe e2 n1
      trans =
        transiciones a1 ++
        transiciones a2 ++
        [(f, Nothing, [estadoInicial a2]) | f <- estadosFinales a1]
      sigma = alfabeto a1 ++ alfabeto a2
      qs = estados a1 ++ estados a2
   in (AFNe qs sigma trans (estadoInicial a1) (estadosFinales a2), n2)

-- Caso: Estrella de Kleene (a*)
construirAFNe (Estrella e) n =
  let (a, n1) = construirAFNe e n
      q0 = nuevoEstado n1
      qf = nuevoEstado (n1 + 1)
      trans =
        transiciones a ++
        [(q0, Nothing, [estadoInicial a, qf])] ++
        [(f, Nothing, [estadoInicial a, qf]) | f <- estadosFinales a]
      qs = q0 : qf : estados a
   in (AFNe qs (alfabeto a) trans q0 [qf], n1 + 2)

-- Caso: Rango de caracteres [a-z]
construirAFNe (Rango a b) n =
  let q0 = nuevoEstado n
      qf = nuevoEstado (n + 1)
      sigma = [a .. b]
      trans = [(q0, Just c, [qf]) | c <- sigma]
   in (AFNe [q0, qf] sigma trans q0 [qf], n + 2)