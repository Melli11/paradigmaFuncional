# TP-3

## Tareas

- [ ] Aceptar el assignment y clonar el repositorio con el ejercicio
- [ ] Ir a la carpeta donde descargaste el ejercicio.
- [ ] Reemplazar la lista de integrantes con los nombres de los integrantes del equipo en el archivo README.md
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

En este TP van a tener que definir **usando recursividad** (no vale usando otras funciones que hagan la recursividad por ustedes) las siguientes funciones y tiparlas y **escribir ustedes los tests**:

- **obtenerElemento**: recibe un numero y una lista, y devuelve el elemento con ese indice en la lista. Ej:
```haskell
> obtenerElemento 0 [1, 2, 3]
1.0
> obtenerElemento 2 [1, 2, 3, 4, 5]
3.0
> obtenerElemento 1 ["hola", "chau"]
"chau"
```

- **sacarHastaEncontrar**: recibe una lista de cartas y una carta, devuelve todas las cartas en la lista hasta encontrar la carta buscada. Ej:
```haskell
> sacarHastaEncontrar (CartaNumerica 5 Rojo) [CartaNumerica 2 Azul, CartaEspecial Reversa Rojo, CartaNumerica 5 Rojo, CartaNumerica 4 Amarillo, CartaNumerica 0 Verde]
[CartaNumerica 2 Azul, CartaEspecial Reversa Rojo, CartaNumerica 5 Rojo]
```
Si la carta no está, devuelve todas las cartas pasadas.

- **lasRojas**: dada una lista de cartas, retorna una lista con solo aquellas que son rojas. Ej:
```haskell
> lasRojas [CartaNumerica 5 Rojo, CartaNumerica 2 Azul, CartaEspecial Reversa Rojo, CartaNumerica 4 Amarillo]
[CartaNumerica 5 Rojo, CartaEspecial Reversa Rojo]
```

- **lasQueSonDeColor**: dada una lista de cartas y un color, retorna una lista con solo aquellas que son de ese color. Ej:
```haskell
> lasQueSonDeColor Azul [CartaNumerica 5 Rojo, CartaNumerica 2 Azul, CartaEspecial Reversa Rojo, CartaNumerica 4 Amarillo]
[CartaNumerica 2 Azul]
```

- **lasFiguras**: dada una lista de cartas, retorna una lista con solo aquellas que tengan figuras. Ej:
```haskell
> lasFiguras [CartaNumerica 5 Rojo, CartaNumerica 2 Azul, CartaEspecial Reversa Rojo, CartaNumerica 4 Amarillo]
[CartaEspecial Reversa Rojo]
```

- **sumatoria**: dado una lista de números, los suma. Si la lista está vacía devuelve 0. No usar `sum` que es literalmente la misma función que queremos definir :). Ej:
```haskell
> sumatoria [1,2,3,4,5,6]
21
> sumatoria []
0
```


## Que hacer cuando terminan el TP

Cuando terminen, creen un issue etiquetando a sus tutores así les llega una notificación y se corrigen y les dejan feedback ahí.
![](https://i.imgur.com/ypeXpBw.gif)
