-- ------------------------------------------------------------
-- Analizador léxico del lenguaje IMP
-- ------------------------------------------------------------

module Lexer (lexer) where

import Tokens
import ER_IMP
import MDD
import ER
import AFNe
import AFN
import AFD
import AFDmin
import Data.Char (isSpace)

-- ------------------------------------------------------------
-- Función principal: recibe un String y devuelve [Token]
-- ------------------------------------------------------------
lexer :: String -> [Token]
lexer [] = [TEOF]
lexer s@(c:cs)
  | isSpace c = lexer cs  -- Ignorar espacios
  | take 2 s == "--" = skipComment cs
  | otherwise = matchToken s

-- ------------------------------------------------------------
-- Salta comentarios
-- ------------------------------------------------------------
skipComment :: String -> [Token]
skipComment input =
  let rest = dropWhile (/= '\n') input
  in TComment "" : lexer (dropWhile (== '\n') rest)

-- ------------------------------------------------------------
-- Intenta emparejar el token más largo posible
-- ------------------------------------------------------------
matchToken :: String -> [Token]
matchToken input =
  case longestMatch input of
    Just (tok, rest) -> tok : lexer rest
    Nothing -> error ("Error léxico: carácter no reconocido: " ++ take 10 input)

-- ------------------------------------------------------------
-- Determina qué token casa al principio de la cadena
-- ------------------------------------------------------------
longestMatch :: String -> Maybe (Token, String)
longestMatch input =
  foldr
    (\(regex, tokBuilder) acc ->
        case reconocer regex input of
          Just (lexema, rest) -> Just (tokBuilder lexema, rest)
          Nothing -> acc)
    Nothing
    tokenTable

-- ------------------------------------------------------------
-- Tabla de tokens: cada ER con su constructor de Token
-- ------------------------------------------------------------
tokenTable :: [(ER, String -> Token)]
tokenTable =
  [ (erEntero, \s -> TInt (read s))
  , (erIdentificador, TId)
  , (erOpAritmetico, toOpArit)
  , (erOpRelacional, toOpRel)
  , (erReservadas, toReserv)
  , (erAsignacion, const TAssign)
  , (erDelimitador, const TSemicolon)
  ]

-- ------------------------------------------------------------
-- Reconoce el prefijo más largo que pertenece al lenguaje ER
-- ------------------------------------------------------------
reconocer :: ER -> String -> Maybe (String, String)
reconocer expr input =
  let afne   = erAafne expr
      afn    = afneToAfn afne
      afd    = afnToAfd afn
      afdmin = afdMinimizar afd
      mdd    = afdminToMDD afdmin
  in longestAcceptedPrefix mdd input

-- ------------------------------------------------------------
-- Simula la MDD y devuelve el prefijo aceptado más largo
-- ------------------------------------------------------------
longestAcceptedPrefix :: MDD -> String -> Maybe (String, String)
longestAcceptedPrefix mdd input =
  go "" (inicialMDD mdd) input Nothing
  where
    go acc estado [] mejor
      | estado `elem` finalesMDD mdd = Just (acc, [])
      | otherwise = mejor

    go acc estado (c:cs) mejor =
      case step estado c of
        Just siguiente ->
          let nuevoMejor =
                if siguiente `elem` finalesMDD mdd
                  then Just (acc ++ [c], cs)
                  else mejor
          in go (acc ++ [c]) siguiente cs nuevoMejor
        Nothing -> mejor

    -- Busca la transición desde un conjunto de estados con un símbolo
    step :: [AFD.Estado] -> Char -> Maybe [AFD.Estado]
    step estadoActual c =
      lookup (estadoActual, c)
        [ ((origen, simbolo), destino)
        | (origen, simbolo, destino) <- transicionesMDD mdd
        ]

-- ------------------------------------------------------------
-- Funciones auxiliares para construir tokens
-- ------------------------------------------------------------
toOpArit :: String -> Token
toOpArit "+" = TPlus
toOpArit "-" = TMinus
toOpArit "*" = TTimes
toOpArit "/" = TDiv
toOpArit _   = error "Operador aritmético desconocido"

toOpRel :: String -> Token
toOpRel "="  = TEquals
toOpRel "<"  = TLt
toOpRel ">"  = TGt
toOpRel "<=" = TLeq
toOpRel ">=" = TGeq
toOpRel _    = error "Operador relacional desconocido"

toReserv :: String -> Token
toReserv "if"    = TIf
toReserv "then"  = TThen
toReserv "else"  = TElse
toReserv "while" = TWhile
toReserv "do"    = TDo
toReserv "not"   = TNot
toReserv "and"   = TAnd
toReserv "or"    = TOr
toReserv "true"  = TTrue
toReserv "false" = TFalse
toReserv "skip"  = TSkip
toReserv _       = error "Palabra reservada desconocida"