# TP-2

## Tareas

- [ ] Aceptar el assignment y clonar el repositorio con el ejercicio
- [ ] Ir a la carpeta donde descargaste la kata. Ejemplo: `cd /home/juan/haskell/2022-funcional-tp-2`. Ejecutar `stack build --test`.
- [ ] Reemplazar la lista de integrantes con los nombres de los integrantes del equipo en el archivo README.md
- [ ] Resolver el ejercicio siguiendo [un esquema de trabajo](https://github.com/pdep-utn/enunciados-miercoles-noche/blob/master/pages/haskell/trabajo.md), eso incluye
- [ ] Ejecutar los tests con `stack test` y que den verde
- [ ] A medida que vas resolviendo el ejercicio, subir [el progreso a git](https://github.com/pdep-utn/enunciados-miercoles-noche/blob/master/pages/git/resolverConflictos.md)

## Nombre y Legajo

- Legajo: 1416534
- Nombre: Martin R Gerez

## Pre-requisitos

Necesitás haber instalado el ambiente según se explica en el [TP-0](https://classroom.github.com/a/u4aiGg9H).

## Ayuda

Si tienen dudas con Haskell pueden ayudarse todo el tiempo con esta documentación

- [Guía de lenguajes](https://docs.google.com/document/d/1oJ-tyQJoBtJh0kFcsV9wSUpgpopjGtoyhJdPUdjFIJQ/edit?usp=sharing), un resumen de las principales funciones que vienen con Haskell.
- [Hoogle](https://www.haskell.org/hoogle/), un motor de búsqueda específico para Haskell.

Aparte, siempre pueden preguntar a sus ayudantes en discord!

Y para comenzar a trabajar con Git les recomendamos [este apunte inicial de Git](https://docs.google.com/document/d/1ozqfYCwt-37stynmgAd5wJlNOFKWYQeIZoeqXpAEs0I/edit) o estos videos donde se explica como usar Git:
- [Parte 1: Qué es GIT y cómo clonar el repo basado en GitHub classroom](https://www.youtube.com/watch?v=rRKe7l-ZNvM)
- [Parte 2: Uso básico de GIT con status, add, reset, commit, push](https://www.youtube.com/watch?v=OgasfM5qJJE)
- [Parte 3: Resolución de conflictos](https://www.youtube.com/watch?v=sKcN7cWFniw)

### Probando cosas por consola

La forma que recomendamos de resolver el ejercicio es no programar todo de una y después ver si anda, si no ir probando en cada paso a medida que van programando cada función.

Para esto, les recomiendo que usen mucho `stack ghci` para probar cosas por consola, y vuelvo a linkear [esta página](https://github.com/pdep-utn/enunciados-miercoles-noche/blob/master/pages/haskell/trabajo.md#comandos-%C3%BAtiles) donde se explican un par de cositas de como usar `ghci`.

### Testeo automatizado

Nuestra solución tiene que estar escrita en el archivo `Library.hs` del directorio `src`, entonces podemos correr pruebas **automatizadas** en la terminal:

```bash
stack test
```

También pueden usar ghcid para correr los tests automáticamente como se explica [acá](https://github.com/pdepviernestm/2021-clases/blob/main/clase-02/correrTestsMasRapidoConGhcid.md).

## El enunciado

En este TP van a tener que tipar todas las funciones ustedes.

### 1. Tuplas

- **first**, **second** y **third**: fst y snd no funcionan con tuplas de 3 parámetros porque una tupla de 3 parámetros y una de dos van a ser de diferente tipo (pueden probarlo en `stack ghci`). Definan las funciones **first**, **second** y **third** que dada una tupla de tres parámetros dan respectivamente el primer, segundo y tercer valor. Por ej.
```
> first (1, "dos", 3)
1
> second (1, "dos", 3)
"dos"
> third (1, "dos", 3)
3
```
- **swap**: recibe una tupla de dos elementos cualquiera y devuelve otra tupla con los elementos cambiados de orden. Por ej.
```
> swap ("hola", 5)
(5, "hola")
```
- **divisionConResto**: las tuplas también son una buena manera de representar una función donde el resultado representa "dos cosas" a la vez. Entonces, definan una función que dado un dividendo y un divisor, te da la división entera entre ellos Y también el resto de esa división entera. Pueden usar `div` y `rem` para implementarla. Ej:
```
> divisionConResto 5 3
(1, 2)
```

### 2. Títulos

![](https://thumbs.gfycat.com/IdioticWickedGalago-size_restricted.gif)

Este punto va a estar un poco menos guiado, acá ustedes van a tener que definir como se crean los valores a usar.

#### Tests

Como van a estar definiendo ustedes como crear los datos, también van a tener que definir ustedes las implementaciones de los tests a realizar. ¿Cómo se hace esto?
En el archivo test/Spec.hs, cada `it "..." $ do` es un test, en la línea de abajo de eso tienen que escribir el cuerpo del mismo. Los nombres de los tests para esta parte y la siguiente van a estar hecha y, así que solo tienen que completar los `it` que están vacíos con un ejemplo de lo que diga el título. El formato de todos los tests que van a escribir va a ser:

```
aplicacion de funcion `shouldBe` resultadoEsperado
```

Ejemplo de un test de puntos anteriores.:

```
 it "divisionConResto de un dividendo no divisible por su divisor, me da el resultado de la division y el resto" $ do
    divisionConResto 4 3 `shouldBe` (1, 1)
```

(divisionConResto 4 3) es el ejemplo a probar, y (1, 1) es el resultado esperado para ese ejemplo.

Nota: para los primeros tests que tienen que escribir ustedes, hay ejemplos que pueden trasladar a su código como comentarios, para los siguientes tienen que pensar ustedes ejemplos que tengan sentido.

#### Implementación

Ahora sí, queremos representar personas que están estudiando diferentes carreras, y nos interesa saber su nombre, apellido, edad y título. Los títulos que queremos registrar en el sistema son Ingenieria, Licenciatura y Doctorado. Tambíen queremos contemplar el caso en el que aun no se tiene un título.

Lo que vamos a querer poder hacer con estudiantes es pedir:

- **edad**: nos da la edad de cualquier estudiante que pasemos.
- **nombreCompleto**: si pasamos a alguien que no tiene título es su nombre + su apellido (separados por espacio), si pasamos a alguien con algún título, se le agrega la abreviación del título adelante. Por ej, si tenemos a Ayelen García que es ingeniera, y le pedimos su nombre completo, nos debería dar "Ing. Ayelen García". Las abreviaciones para cada título son:
  - Ingenieria -> Ing.
  - Licenciatura -> Lic.
  - Doctorado -> Doc.
- **recibirse**: recibe como parámetro un título y a una persona, y retorna a la persona luego de recibirse, que es la persona con el título que se pasó por parámetro e incrementando su edad en los años que tarda el título en obtenerse.
Para los títulos que tenemos, los tiempos son:
  - Ingenieria -> 6 años
  - Licenciatura -> 4 años
  - Doctorado -> 2 años
  
Nota: Si pasamos como parámetro a alguien que ya tiene un título, solo se reemplaza por el título más nuevo, así que no es necesario chequear que no tenga título para empezar.

### 3. Pregunta:

Si yo tengo a un estudiante que tiene 26 años, se llama Juan Fernandes y tiene título de Ingeniería, y en `ghci` evaluo el código para obtener a juan recibido con un doctorado y evaluo `edad juan`, ¿cuantos años me va a dar? ¿Por qué?
(contestar eso en un comentario en el archivo)

### 4. Punto BONUS: Devs (esto es opcional)

Para realizar este punto va a haber que modificar un poco lo realizado en el punto 2. Se agrega un nuevo tipo de título, `Dev` (Developer).

El tema es que cuando se habla de este título siempre se lo acompaña de la experiencia que se tiene trabajando en el área, así que el título debería ir acompañado de años de experiencia. No es lo mismo el título de Dev con 0 años de experiencia que el título de Dev con 5 años de experiencia.

Recibirse de Dev con 0 años de experiencia lleva 2 años de estudio. Pero recibirse de Dev con más años de experiencia, lleva 2 años de estudio + esa cantidad de años de experiencia (lleva más recibirse cuando estás trabajando `:(`).

Además, la abreviación que se agrega al nombre depende de la cantidad de años de experiencia que se lleva como Dev:
Para Devs con 0 o 1 años de experiencia, la abreviación que se usa es "Dev Jr.", para Devs con más de 1 pero menos de 5 años de experiencia, la abreviación es "Dev Ssr.".
Para Devs con más de 5 años de experiencia se usa "Dev Sr.".

Finalmente, se puede ganar experiencia luego de haberse recibido de Dev, así que queremos plasmar esto con la siguiente función:

- **practicar**: que recibe una persona y una cantidad de años de práctica y nos devuelve a la persona envejecida en esa cantidad de años y aparte:
  - si era Dev, aumenta sus años de experiencia en esos años de práctica.
  - si su título era cualquier otro o no tenía, no cambia nada más.

## Que hacer cuando terminan el TP

Cuando terminen, creen un issue etiquetando a sus tutores así les llega una notificación y se corrigen y les dejan feedback ahí.
![](https://i.imgur.com/ypeXpBw.gif)
