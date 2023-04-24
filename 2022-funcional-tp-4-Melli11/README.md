# TP-4

## Tareas

- [ ] Aceptar el assignment y clonar el repositorio con el ejercicio
- [ ] Ir a la carpeta donde descargaste el ejercicio.
- [ ] Reemplazar nombre y legajo en el archivo README.md
- [ ] Resolver el ejercicio siguiendo [un esquema de trabajo](https://github.com/pdep-utn/enunciados-miercoles-noche/blob/master/pages/haskell/trabajo.md), eso incluye
- [ ] Ejecutar los tests con `stack test` y que den verde
- [ ] A medida que vas resolviendo el ejercicio, subir [el progreso a git](https://github.com/pdep-utn/enunciados-miercoles-noche/blob/master/pages/git/resolverConflictos.md)

## Nombre y Legajo

- Legajo: **1416534**
- Nombre: **Martin Gerez**

## Pre-requisitos

Necesitás haber instalado el ambiente según se explica en el TP-0.

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

Resolver este TP  **usando funciones de orden superior**, tienen que tipar y definir todas las funciones, y escribir tests para las mismas (la parte 4 es opcional).

### Parte 1: Animales

Dado el siguiente tipo de datos: 

```haskell
data Animal = Animal{
    energia :: Number,
    tipo :: Tipo,
    peso :: Number
} deriving(Show)

data Tipo = Volador| Terrestre | Acuatico deriving(Show)
```

Vamos a querer modelar y tipar: 

### PARTE 1: Animales
- **losDeTipo**: recibe un tipo y una lista de animales y nos devuelve aquellos que son de ese tipo. Por Ej:

```haskell
> tigre = Animal 5 Terrestre 120
> lechuza = Animal 40 Volador 10
> tiburon = Animal 100 Acuatico 100
> losDeTipo Acuatico [tigre, lechuza, tiburon]
[tiburon]
```

- **animalesHambrientos**: dada una lista de animales, nos devuelve solo aquellos que tienen hambre. Que un animal tenga hambre significa que su energía es menor a 10.

- **entrenar**:
- Si el animal es terrestre disminuye el peso y la energia en 5.
- Si el animal es volador solo disminuye su peso en 3.
- Si el animal es acuatico no hace nada porque es re jodido entrenar un animal acuatico.

    Por ej.:
    ```haskell
       > entrenar (Animal 140 Terrestre 30)
       Animal 135 Terrestre 25
       > entrenar (Animal 100 Acuatico 100)
       Animal 100 Acuatico 100
    ```

### PARTE 2: Alimentos y entrenamientos

Queremos modelar los siguientes alimentos:
- **baya** : aumenta en 5 la energía y el peso en 0.1
- **carne** : aumenta en 20 la energía y el peso en 2

- **alimentarATodos**: Recibe un alimento y una lista de animales y los alimenta a todos. Ej:
```haskell
> alimentarATodos baya [Animal 100 Acuatico 100, Animal 30 Terrestre 20]
[Animal 105 Acuatico 100.1, Animal 35 Terrestre 20.1]
```

- **aplicarItinerario** : Dado un animal y una lista de alimentos y entrenamietos se los aplica a todos en orden (haciendo que el animal sufra todos los efectos). Ej: hacer comer una baya, comer carne, luego entrenar 2 veces y finalmente comer otra baya a un leon(energia = 40, tipo = Terrestre, peso = 200) me dejaria un leon(energia= 60, tipo = Terrestre, peso= 192.2). Ejemplo en codigo:
    ```haskell
    > aplicarItinerario [entrenar, entrenar] (Animal 25 Terrestre 120)
    Animal 15 Terrestre 110
    ```
    Como poner en la lista del itinerario que coma una baya o que coma carne depende de como hayan modelado las bayas y la carne.

### PARTE 3: Nuestras propias funciones de orden superior

En este punto ustedes van a definir funciones que reciban funciones por parámetro:

- **mapTupla**: recibe una función y una tupla de dos elementos del mismo tipo, y devuelve una tupla con los resultados de aplicar la función a cada valor:
    ```haskell
    > mapTupla length ("hola", "mundo")
    (4, 5)
    ```

- **menorSegun**: recibe una función y dos valores del mismo tipo. menorSegun devuelve aquel valor cuyo resultado al ser aplicado a la función es mayor. Por ej:
    ```haskell
    > menorSegun length "hola" "mundo"
    "hola" -- porque (length "hola") es 4 y (length "mundo") es 5
    > menorSegun even 4 3
    3 -- porque False es menor que True en Haskell
    > menorSegun length "hola" "chau"
    4 -- si ambos dan lo mismo, devolver el primero
    ```

- **minimoSegun**: dada una lista y una función, devuelve el menor de toda la lista según la función que se pasó. Por ej:
    ```haskell
    > minimoSegun length ["hola", "ornitorrinco", "a"]
    "a"
    > minimoSegun energia [tigre, lechuza, tiburon]
    Animal 5 Terrestre 120 -- el tigre
    ```

- **aplicarVeces**: dada un número de veces a aplicar, una funcion y un valor. Aplica el valor como parametro a la funcion tantas veces como dice el número.

    Por ej, si la función fuese `siguiente`, el numero de veces fuese 2 y el valor fuese 1, sería lo mismo que hacer: `(siguiente (siguiente 1))`.
    
    Si el número de veces fuese 3: `(siguiente (siguiente (siguiente 1)))`.
    
    Ejemplo con código:
    ```haskell
    > siguiente n = n + 1
    > aplicarVeces 3 siguiente 1
    4
    > aplicarVeces 3 (\n -> n * 2) 1
    8
    > aplicarVeces 2 (\texto -> texto ++ "!") "hola"
    "hola!!"
    ```


- **replicar**: dado un numero y un valor, devuelve una lista con el valor dentro tantas veces como dice el número. Por ej:
    ```haskell
    > replicar 3 "hola"
    ["hola", "hola", "hola"]
    > replicar 2 True
    [True, True]
    ```
    Esta no es una función de orden superior, pero queremos que la definan usando alguna de las funciones de orden superior que ustedes definieron.

### PARTE 4: Bonus (OPCIONAL). Combinando funciones

En esta parte vamos a aprovechar el orden superior para definir maneras mas interesantes de aplicar funciones que:

```haskell
funcion (otraFuncion (otraFuncionMas (valor)))
```

Como primer cosa, queremos implementar el operador **|>**, que dado un valor a su izquierda y una función a su derecha, nos da el resultado de aplicar el valor a la función, por ejemplo:

```haskell
> "hola" |> length
4
> 5 |> (\n -> n + 3)
8
```

Una vez que tenemos esto definido, vamos a definir un par de funciones para trabajar con texto:

- **esVocal**: dado un caracter, nos devuelve True si es a, e, i, o, u, A, E, I, O, o U y False si es cualquier otro caracter. Ojo, los caracteres y los strings en haskell se escriben distinto, con comillas dobles "" escriben un string (o lista de caracteres) y con comillas simples '' escriben un caracter. Ejemplo:
    ```haskell
    > esVocal 'a'
    True
    > esVocal 'f'
    False
    ```
    Pista: pueden usar `elem` (busquenla en la guía de lenguajes) para no tener que repetir demasiados chequeos.

- **primeraLinea**: dado un string, me devuelve ese string hasta el primer caracter de salto de linea (\n) que se encuentre. Pista: pueden usar `takeWhile` que recibe una condicion y una lista y devuelve una lista de todos los elementos hasta antes del primero que no cumple la condición. Ejemplo:
    ```haskell
    > primeraLinea "hola\nmundo"
    "hola"
    > primeraLinea "como\nva\neso?"
    "como"
    ```

- **lasVocales**: dado un string, me devuelve otro string con las vocales del primero en orden. Ejemplo:
    ```haskell
    > lasVocales "murcielago"
    "uieao"
    > lasVocales "Aeea yo soy sabalero"
    "Aeeaooaaeo"
    ```

- **contarVocalesDeLaPrimerLinea**: dado un string, me devuelve la cantidad de vocales que se encuentran en la primer linea. Ejemplo
    ```haskell
    > contarVocalesDeLaPrimerLinea "Aeea,\n yo soy sabalero"
    4
    > contarVocalesDeLaPrimerLinea "hola\ncomo\nva"
    2
    ```
    Esta función requiere que usen las que definieron acá arriba (y también length), pero traten de definirla tanto aplicando una función atrás de otra con paréntesis, como con `|>` de esta manera:

    `valor |> primeraFuncion |> segundaFuncion`

    como para que puedan comparar como queda.

    Casi que acabamos de definir sintaxis nueva para el lenguaje!



## Que hacer cuando terminan el TP

Cuando terminen, creen un issue etiquetando a sus tutores así les llega una notificación y se corrigen y les dejan feedback ahí.
![](https://i.imgur.com/ypeXpBw.gif)
