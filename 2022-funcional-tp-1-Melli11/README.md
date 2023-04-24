# TP-1

## Tareas

- [ ] Aceptar el assignment y clonar el repositorio con el ejercicio
- [ ] Ir a la carpeta donde descargaste la kata. Ejemplo: `cd /home/juan/haskell/2022-funcional-tp-1`. Ejecutar `stack build --test`.
- [ ] Reemplazar nombre y legajo con tu nombre y legajo en el  README.md
- [ ] Resolver el ejercicio siguiendo [un esquema de trabajo](https://github.com/pdep-utn/enunciados-miercoles-noche/blob/master/pages/haskell/trabajo.md), eso incluye
- [ ] Ejecutar los tests con `stack test` y que den verde
- [ ] A medida que vas resolviendo el ejercicio, subir [el progreso a git](https://github.com/pdep-utn/enunciados-miercoles-noche/blob/master/pages/git/resolverConflictos.md)

## Nombre y Legajo

- Legajo: 1416534 
- Nombre: Martin Gerez

## Objetivos

El primer TP que preparamos para que resuelvan tiene como objetivos:

- que se familiaricen con el uso del lenguaje Haskell y de la herramienta Visual Studio Code
- que entiendan el circuito básico del desarrollo de un ejercicio, desde bajar el enunciado desde un repositorio hasta completar cada parte e ir subiendo la solución en progreso en cada paso
- utilizar pruebas automatizadas para facilitar la validación del código que escriben

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

También pueden ejecutar una sesión interactiva en la terminal lo cual hace que los tests se vuelvan a correr solos cada vez que guardas!: `stack test --file-watch`, como muestra [esta página](https://github.com/pdep-utn/enunciados-miercoles-noche/blob/master/pages/haskell/trabajo.md#pruebas-automatizadas).

Para conocer un poco más del testeo unitario automatizado recomendamos leer [este apunte](https://docs.google.com/document/d/17EPSZSw7oY_Rv2VjEX2kMZDFklMOcDVVxyve9HSG0mE/edit#)

## El enunciado

Este ejercicio está partido en 4 partes que van aumentando de a poco en complejidad. En cada uno van a tener que implementar las definiciones de algunas funciones para las que ya les vamos a dar tests que prueben que su solución esta funcionando correctamente y (en la mayoría de los casos) los tipos. Algunas de las funciones además de pedirles la implementación también les vamos a pedir que definan sus tipos.

1. Números

Empecemos definiendo algunas funciones simples sobre números:

- **siguiente**: toma un número como parámetro y devuelve ese número + 1.
- **esPositivo**: nos dice si un número es mayor a 0 o no. Es falso para 0.
- **inversa**: aplicar la inversa a un número nos da como resultado 1 dividido ese número. Por ejemplo, la inversa de 2 es 0.5 porque 1/2 es 0.5. **Para está función, también tienen que escribir el tipo ustedes**

2. Temperaturas

Las temperaturas en diferentes países se miden en grados, pero no todos usan el mismo sistema de medida. En la mayor partedel el mundo usamos los grados Celsius (°C) pero en algunos países como en Estados Unidos se usan los grados Farenheit (°F).
¿Cómo se pasa de celsius a farenheit? Usando la siguiente formula:

```
GradosFarenheit = GradosCelsius × 1.8 + 32
```

Y para el otro lado se puede obtener despejando GradosCelsius:

```
GradosCelsius = (GradosFarenheit - 32) / 1.8
```

Entonces, vamos a implementar unas funciones que hagan esto por nosotros:

- **celsiusAFarenheit**: toma una temperatura (que la vamos a representar como un número) en celsius, y la convierte a farenheit. 
- **farenheitACelsius**: está función hace el camino inverso a la anterior, le damos una temperatura en farenheit y la pasa a celsius.

Ahora, queremos una función a la cuál podamos preguntarle si hace frío o no. Nuestro criterio va a ser que hace frío sólo si la temperatura es menor o igual a **8 grados Celsius**. Entonces, definamos:

- **haceFrioCelsius**: recibe una temperatura (en Celsius) y debería ser verdadero si hace igual o menos de 8 grados Celsius.
- **haceFrioFarenheit**: ésta recibe la temperatura como parámetro en Farenheit, pero el criterio para saber si hace frío es el mismo de antes.

**Para `haceFrioCelsius` y `haceFrioFarenheit` también tienen que escribir el tipo ustedes**.

3. Más números!

Acá vamos a definir un par de funciones muy útiles, `max` y `min`.
- **max** toma dos valores como parámetro y nos devuelve aquel que sea mas grande
- **min** toma lo mismo que max pero nos devuelve el que sea mas chico.
Entonces, yo puedo hacer `min 2 3` y el resultado sería `2`, y si hago `max 3 15.5` el resultado debería ser 15.5.

Como estas funciones ya existen en Haskell, pero queremos definirlas igual para practicar, vamos a ponerles de nombre `max'` y `min'` (el ' es un caracter valido para las funciones en Haskell), y en este ejercicio no vale definirlas como `max' = max` ;).

4. Pinos

En una plantación de pinos, de cada árbol se conoce la altura expresada en metros. El peso de un pino se puede calcular a partir de la altura así:

  - 3 kg por cada centímetro hasta 3 metros,
  - 2 kg por cada centímetro arriba de los 3 metros. 

![](https://raw.githubusercontent.com/MumukiProject/mumuki-guia-funcional-practica-valores-y-funciones/master/images/pino.png)

Por ejemplo:

  - 2 metros pesan 600 kg, porque 200 * 3 = 600
  - 5 metros pesan 1300 kg, porque los primeros 3 metros pesan 900 kg y los siguientes 2 pesan los 400 restantes. 
    
![](https://raw.githubusercontent.com/MumukiProject/mumuki-guia-funcional-practica-valores-y-funciones/master/images/pinos.png)

Los pinos se usan para llevarlos a una fábrica de muebles, a la que le sirven árboles de entre 400 y 1000 kilos, un pino fuera de este rango no le sirve a la fábrica. Entonces, un pino de 2 metros serviría porque pesa 600kg, pero un pino de 5 metros no serviría porque pesa 1300kg.

Vamos a definir las siguientes funciones, y en este punto, todas van a ser tipadas por ustedes:

- **pesoPino**: recibe la altura de un pino en metros y devuelve su peso.
- **esPesoUtil**: recibe un peso en kg y responde si un pino de ese peso le sirve a la fábrica
- **sirvePino**: recibe la altura de un pino y responde si un pino de ese peso le sirve a la fábrica. 


## Que hacer cuando terminan el TP

Cuando terminen, creen un issue etiquetando a sus tutores así les llega una notificación y se corrigen y les dejan feedback ahí.
![](https://i.imgur.com/ypeXpBw.gif)
