# Haskell_Interpreter
College project to learn haskell and basics of compilers.

Para compilar la práctica:

	make

Esto debería de dar permiso de ejecución al script test.sh
En caso en que no, ejecutar:

	chmod 755 test.sh


Para interpretar un programa:

	./test.sh program

Donde program es un archivo que contiene el programa a interpretar
Si el parser falla, imprimirá el error

A continuación se iniciará la ejecución del programa. El usuario tendrá que introducir el tipo de datos (int o double), el tipo de ejecución, la semilla aleatoria y el número de tests en el caso del test múltiple. Estos datos se introducen con un número por línea

Posteriormente, se tendrá que introducir la lista de input que recibirá el programa a interpretar. Esta lista se define con números del tipo seleccionado en la misma línea, separados por un espacio

El usuario puede escribir todo el input con el formato descrito anteriormente (opciones + lista) en un archivo de texto y ejecutarlo de la siguiente forma:

	./test.sh program input

Donde input es el archivo mencionado



Nota: No he implementado el contador de instrucciones porque no sé cómo hacerlo sin modificar las funciones del apartado 1
