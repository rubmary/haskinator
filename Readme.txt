Proyecto I: Haskinator
CI 3661 - Lenguajes de Programación
Rubmary Rojas 13-11264, Carlos Infante 13-10681

El módulo Oraculo cuenta exclusivamente con las funciones y los tipos de datos requeridas.

El módulo Haskinator realiza el resto del manejo del programa, y exporta la función "main".

El programa se compila usando el makefile, que genera un ejecutable "haskinator". El programa
muestra al siguiente menú:

Introduzca una opción.
1. Crear un oráculo nuevo
2. Predecir
3. Persistir
4. Cargar
5. Consultar pregunta crucial
6. Salir

Entonces, el usuario debe introducir el número correspondiente a la opción a la que desea acceder.
Si no existe ningún oráculo cargado, entonces el usuario solo podrá seleccionar las opciones 1, 4, y 6.
De lo contrario, se mostrará un error, y se pedirá al usuario una nueva opción.

En caso de que el usuario quiera predecir en el oráculo actual, se planteará la serie de preguntas, hasta
llegar a una predicción. 

En caso de que el usuario introduzca "ninguna" (en minúsculas) como respuesta de
una pregunta, se le permitirá agregar una nueva opción que lleve hacia la predicción correcta.

En caso de que la predicción sea incorrecta, se le permitirá al usuario agregar una nueva pregunta
que permita llegar a la predicción correcta.

En cualquiera de los dos casos, al usuario no se le permite introducir ninguna opción repetida, y tampoco
se le permite introducir "ninguna" como opción. Si esto ocurre, se le permitirá al usuario introducir una
opción nuevamente.

Cualquier cadena de caracteres introducida, será "case sensitive". Esto es: "a" =/= "A".

Si el usuario intenta cargar un archivo que no existe, ocurrirá un error y se saldrá de la ejecución del programa.