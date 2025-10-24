module ER_IMP where
import ER

-- ------------------------------------------------------------
-- Expresiones regulares del lenguaje IMP
-- ------------------------------------------------------------

-- Enteros: (0 + (1-9)(0-9)* + -(1-9)(0-9)*)
erEntero :: ER
erEntero =
  Union
    (Simbolo '0')
    (Union
      (Concatenacion (Rango '1' '9') (Estrella (Rango '0' '9')))
      (Concatenacion
        (Simbolo '-')
        (Concatenacion (Rango '1' '9') (Estrella (Rango '0' '9')))
      )
    )

-- Identificadores: ((a–z + A–Z) (a–z + A–Z + 0–9)*)
erIdentificador :: ER
erIdentificador =
  Concatenacion
    (Union (Rango 'a' 'z') (Rango 'A' 'Z'))
    (Estrella (Union (Union (Rango 'a' 'z') (Rango 'A' 'Z')) (Rango '0' '9')))

-- Operadores aritméticos: (+ + - + * + /)
erOpAritmetico :: ER
erOpAritmetico =
  Union (Simbolo '+')
        (Union (Simbolo '-') (Union (Simbolo '*') (Simbolo '/')))

-- Operadores relacionales: (= + < + > + <= + >=)
erOpRelacional :: ER
erOpRelacional =
  foldr1 Union
    [ Simbolo '='
    , Simbolo '<'
    , Simbolo '>'
    , stringToER "<="
    , stringToER ">="
    ]

-- Palabras reservadas
erIf, erThen, erElse, erWhile, erDo, erNot, erAnd, erOr, erTrue, erFalse, erSkip :: ER
erIf    = stringToER "if"
erThen  = stringToER "then"
erElse  = stringToER "else"
erWhile = stringToER "while"
erDo    = stringToER "do"
erNot   = stringToER "not"
erAnd   = stringToER "and"
erOr    = stringToER "or"
erTrue  = stringToER "true"
erFalse = stringToER "false"
erSkip  = stringToER "skip"

erReservadas :: ER
erReservadas =
  foldr1 Union [erIf, erThen, erElse, erWhile, erDo, erNot, erAnd, erOr, erTrue, erFalse, erSkip]

-- Asignación: :=
erAsignacion :: ER
erAsignacion = stringToER ":="

-- Delimitadores
erDelimitador :: ER
erDelimitador = Simbolo ';'

-- Blancos (espacio, tab, salto de línea, retorno)
erBlanco :: ER
erBlanco =
  foldr1 Union
    [ Simbolo ' '
    , Simbolo '\t'
    , Simbolo '\n'
    , Simbolo '\r'
    ]

-- Fin de archivo (opcional)
erEOF :: ER
erEOF = stringToER "eof"

-- Comentarios: -- seguido de cualquier secuencia no vacía que no contenga salto de línea
erComentario :: ER
erComentario =
  Concatenacion
    (Concatenacion (Simbolo '-') (Simbolo '-'))
    (Estrella (Union (Rango 'A' 'Z') (Union (Rango 'a' 'z') (Rango '0' '9'))))

-- -----------------------------------------------------------
-- Función auxiliar: convertir String a ER
-- ------------------------------------------------------------

stringToER :: String -> ER
stringToER []     = Epsilon
stringToER [c]    = Simbolo c
stringToER (c:cs) = Concatenacion (Simbolo c) (stringToER cs)

-- -----------------------------------------------------------
-- Unión de todas las expresiones regulares
-- -----------------------------------------------------------

erIMP :: ER
erIMP = foldr1 Union
  [ erEntero
  , erIdentificador
  , erOpAritmetico
  , erOpRelacional
  , erReservadas
  , erAsignacion
  , erDelimitador
  , erBlanco
  , erComentario
  , erEOF
  ]