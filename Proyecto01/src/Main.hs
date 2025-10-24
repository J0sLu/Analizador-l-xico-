module Main where

import ER
import AFNe
import AFN
import AFD
import AFDmin
import MDD
import ER_IMP
import Tokens
import Lexer

main :: IO ()
main = do
  -- Definición de expresiones regulares de prueba
  let e1 = Simbolo 'a'
  let e2 = Simbolo 'b'
  let e3 = Union e1 e2
  let e4 = Concatenacion e1 e2
  let e5 = Estrella e1
  let e6 = Rango 'a' 'c'

  -- Pruebas de ER
  putStrLn "--------------------------------------"
  putStrLn "Pruebas de ER:"
  putStrLn "--------------------------------------"
  print e1
  print e3
  print e4
  print e5
  print e6

  -- Pruebas de AFNe
  putStrLn "\n--------------------------------------"
  putStrLn "Pruebas de AFNe:"
  putStrLn "--------------------------------------"
  let afne1 = erAafne e1
  let afne2 = erAafne e3
  print afne1
  print afne2

  -- Pruebas de AFN
  putStrLn "\n--------------------------------------"
  putStrLn "Pruebas de AFN:"
  putStrLn "--------------------------------------"
  let afn1 = afneToAfn afne1
  let afn2 = afneToAfn afne2
  print afn1
  print afn2

  -- Pruebas de AFD
  putStrLn "\n--------------------------------------"
  putStrLn "Pruebas de AFD:"
  putStrLn "--------------------------------------"
  let afd1 = afnToAfd afn1
  let afd2 = afnToAfd afn2
  print afd1
  print afd2

  -- Pruebas de AFDmin
  putStrLn "\n--------------------------------------"
  putStrLn "Pruebas de AFDmin:"
  putStrLn "--------------------------------------"
  let afdmin1 = afdMinimizar afd1
  let afdmin2 = afdMinimizar afd2
  print afdmin1
  print afdmin2

  -- Pruebas de MDD
  putStrLn "\n--------------------------------------"
  putStrLn "Pruebas de MDD:"
  putStrLn "--------------------------------------"
  let mdd1 = afdminToMDD afdmin1
  let mdd2 = afdminToMDD afdmin2
  print mdd1
  print mdd2

  -- Ejecución de MDD con algunas cadenas de prueba
  putStrLn "\n--------------------------------------"
  putStrLn "Ejecución de MDD con cadenas de prueba:"
  putStrLn "--------------------------------------"
  putStrLn $ "MDD1 acepta 'a'?  " ++ show (ejecutarMDD mdd1 "a")
  putStrLn $ "MDD1 acepta 'b'?  " ++ show (ejecutarMDD mdd1 "b")
  putStrLn $ "MDD2 acepta 'a'?  " ++ show (ejecutarMDD mdd2 "a")
  putStrLn $ "MDD2 acepta 'b'?  " ++ show (ejecutarMDD mdd2 "b")
  putStrLn $ "MDD2 acepta 'ab'? " ++ show (ejecutarMDD mdd2 "ab")
  putStrLn $ "MDD2 acepta 'ba'? " ++ show (ejecutarMDD mdd2 "ba")

  -- Pruebas de ER_IMP
  putStrLn "\n--------------------------------------"
  putStrLn "Pruebas de ER_IMP:"
  putStrLn "--------------------------------------"
  putStrLn "\nExpresión regular para enteros:"
  print erEntero

  putStrLn "\nExpresión regular para identificadores:"
  print erIdentificador

  putStrLn "\nExpresión regular para operadores aritméticos:"
  print erOpAritmetico

  putStrLn "\nExpresión regular para operadores relacionales:"
  print erOpRelacional

  putStrLn "\nExpresión regular para palabras reservadas:"
  print erReservadas

  putStrLn "\nExpresión regular para asignación:"
  print erAsignacion

  putStrLn "\nExpresión regular para delimitadores:"
  print erDelimitador

  putStrLn "\nExpresión regular para comentarios:"
  print erComentario

  putStrLn "\nExpresión regular global del lenguaje IMP:"
  print erIMP

  -- Pruebas de Tokens y Lexer
  putStrLn "\n--------------------------------------"
  putStrLn "Pruebas del Analizador Léxico (Lexer):"
  putStrLn "--------------------------------------"
  let codigo1 = "if x := 3 + 5; while y < 10 do skip;"
  putStrLn "\nCódigo fuente:"
  putStrLn codigo1
  putStrLn "\nTokens generados:"
  print (lexer codigo1)

  let codigo2 = "x := -42; if x > 0 then skip else y := y - 1;"
  putStrLn "\nCódigo fuente:"
  putStrLn codigo2
  putStrLn "\nTokens generados:"
  print (lexer codigo2)