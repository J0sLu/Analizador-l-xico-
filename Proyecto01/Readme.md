
- Instrucciones de Ejecucion -
  
- Ejecutar los siguiente comandos en la ubicacion de la carpeta src:

1. ghci

2. :l ER.hs ER_IMP.hs AFNe.hs AFN.hs AFD.hs AFDmin.hs MDD.hs Tokens.hs Lexer.hs Main.hs

3. :set -main-is Main.main

4. :l main

5. main

Notas:

-El programa tarda en compilar

-Al ejecutarse, mostrará todas las pruebas hechas, desde la creación de ER, hasta los tokens generados por el lexer al darle un pequeño código de prueba.

-Por cuestiones tecnicas, no se pudo implementar una funcion para pasar como argumento un programa de imp al lexer, sin embargo, si se quiere probar el le lexer con codigos diferentes, en el archivo main.hs, linea 124, se puede cambiar la variable codigo1 por el programa de ejemplo que se quiera usar, para obtener los tokens.

- Se quiso implementar un modulo extra para no tener que copiar el comando tan largo que carga los modulos pero al intentarlo nos resulto en muchos errores, asi que decidimos dejarlo asi.
