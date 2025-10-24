module ER where

-- ------------------------------------------------------------
-- Definición de tipo de dato para Expresiones Regulares
-- ------------------------------------------------------------

data ER
  = Vacio -- Representa el conjunto vacío ∅
  | Epsilon -- Representa la cadena vacía ε
  | Simbolo Char -- Representa un símbolo terminal
  | Union ER ER -- Representa la unión de dos lenguajes
  | Concatenacion ER ER -- Representa la concatenación de dos expresiones
  | Estrella ER -- Representa el operador de la Estrella de Kleene
  | Rango Char Char -- Representa un rango de caracteres
  deriving (Eq)

instance Show ER where
  show Vacio = "vacio"
  show Epsilon = "epsilon"
  show (Simbolo c) = [c]
  show (Union l r) = "(" ++ show l ++ " + " ++ show r ++ ")"
  show (Concatenacion l r) = "(" ++ show l ++ " . " ++ show r ++ ")"
  show (Estrella e) = show e ++ "*"
  show (Rango a b) = "[" ++ [a] ++ "-" ++ [b] ++ "]"