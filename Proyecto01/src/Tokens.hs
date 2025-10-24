module Tokens where

-- ------------------------------------------------------------
-- Definición de los tokens para el lenguaje IMP
-- ------------------------------------------------------------

data Token
  -- Palabras reservadas y estructuras de control
  = TIf
  | TThen
  | TElse
  | TWhile
  | TDo
  | TSkip

  -- Booleanos y operadores lógicos
  | TTrue
  | TFalse
  | TNot
  | TAnd
  | TOr

  -- Identificadores y números enteros
  | TId String
  | TInt Int

  -- Operadores aritméticos
  | TPlus
  | TMinus
  | TTimes
  | TDiv

  -- Operadores relacionales
  | TEquals
  | TLeq
  | TGeq
  | TLt
  | TGt

  -- Asignación
  | TAssign        -- :=

  -- Secuencias y delimitadores
  | TSemicolon     -- ;
  | TParenOpen     -- (
  | TParenClose    -- )

  -- Espacios, comentarios y fin de entrada
  | TWhitespace
  | TComment String
  | TEOF
  deriving (Show, Eq)