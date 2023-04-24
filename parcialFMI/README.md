# Parcial FMI

**FMI (Funcional Mata Imperativo)**

![Aspose Words 8e6c8111-c67d-4382-97b4-c000a8aa76ce 001](https://user-images.githubusercontent.com/54424951/169203072-a52a9cf1-7a60-4ddd-a656-63898ef57f59.png)

**Temas a evaluar:**

- Composición 
- Aplicación parcial 
- Orden superior 
- Modelado de información 
- Efecto colateral 
- Recursividad 
- Evaluación diferida 
- Sistema de tipos 

**Enunciado general**

El fondo monetario internacional nos solicitó que modelemos su negocio, basado en realizar préstamos a países en apuros financieros. Sabemos de cada país el *"ingreso per cápita"* que es el promedio de lo que cada habitante necesita para subsistir, también conocemos la población activa en el sector público y la activa en el sector privado, la lista de recursos naturales (ej: "Minería", "Petróleo", "Industria pesada") y la deuda que mantiene con el FMI.

El FMI es especialista en dar recetas. Cada receta combina una o más estrategias que se describen a continuación:

- prestarle n millones de dólares al país, esto provoca que el país se endeude en un 150% de lo que el FMI le presta (por los intereses)
- reducir x cantidad de puestos de trabajo del sector público, lo que provoca que se reduzca la cantidad de activos en el sector público y además que el ingreso per cápita disminuya en 20% si los puestos de trabajo son más de 100 ó 15% en caso contrario
- darle a una empresa afín al FMI la explotación de alguno de los recursos naturales, esto disminuye 2 millones de dólares la deuda que el país mantiene con el FMI pero también deja momentáneamente sin recurso natural a dicho país. No considerar qué pasa si el país no tiene dicho recurso.
- establecer un “blindaje”, lo que provoca prestarle a dicho país la mitad de su Producto Bruto Interno (que se calcula como el ingreso per cápita multiplicado por su población activa, sumando puestos públicos y privados de trabajo) y reducir 500 puestos de trabajo del sector público. Evitar la repetición de código.

Se pide implementar en Haskell los siguientes requerimientos explicitando el tipo de cada función:

1. *(2 puntos)*
   
   1. Representar el TAD País.
   2. Dar un ejemplo de cómo generar al país Namibia, cuyo ingreso per cápita es de 4140 u$$, la población activa del sector público es de 400.000, la población activa del sector privado es de 650.000, su riqueza es la minería y el ecoturismo y le debe 50 (millones de u$$) al FMI.
   
2. *(4 puntos)* Implementar las estrategias que forman parte de las recetas del FMI.
   
3. *(2 puntos)*
   1. Modelar una receta que consista en prestar 200 millones, y darle a una empresa X la explotación de la “Minería” de un país.
   2. Ahora queremos aplicar la receta del punto 3.a al país Namibia (creado en el punto 1.b). Justificar cómo se logra el efecto colateral.
   
4. *(3 puntos) Resolver todo el punto con orden superior, composición y aplicación parcial*, no puede utilizar funciones auxiliares.
   
   1. Dada una lista de países conocer cuáles son los que pueden zafar, aquellos que tienen "Petróleo" entre sus riquezas naturales.
   
   2. Dada una lista de países, saber el total de deuda que el FMI tiene a su favor.
   
   3.  Indicar en dónde apareció cada uno de los conceptos (solo una vez) y justificar qué ventaja tuvo para resolver el requerimiento.
   
5.  *(2 puntos) Debe resolver este punto con recursividad*: dado un país y una lista de recetas, saber si la lista de recetas está ordenada de “peor” a “mejor”, en base al siguiente criterio: si aplicamos una a una cada receta, el PBI del país va de menor a mayor. Recordamos que el Producto Bruto Interno surge de multiplicar el ingreso per cápita por la población activa (privada y pública).
   
6.  *(1 punto)* Si un país tiene infinitos recursos naturales, modelado con esta función 

    **recursosNaturalesInfinitos :: [String]**

    **recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos**

    1. ¿qué sucede evaluamos la función 4a con ese país?
    2. ¿y con la 4b?

Justifique ambos puntos relacionándolos con algún concepto.

**Recordatorio:** Solo puede utilizar recursividad en el punto 5.b. 

