module Library where
import PdePreludat

{-
Dominio
Una persona tiene
●	edad
●	sueños que quiere cumplir (ver punto 3)
●	un nombre con el que se identifica
●	los felicidonios, un número que cuantifica el nivel de felicidad que siente (debe ser positivo)
●	y las habilidades que tiene, como “Pintura”, “Ser buena persona” o “Decir palíndromos”
-}

data Persona = Persona {    
    edad :: Number,
    listaDeSueños :: [Suenio],
    nombre :: String,
    felicidonios :: Number,
    listaDeHabilidades :: [String] 
}   deriving (Show)

{-
Punto 1
Punto a (integrante 1): Coeficiente de satisfacción
Saber el coeficiente de satisfacción de una persona
●	Si los felicidonios son > a 100, son los felicidonios * la edad
●	Si los felicidonios son <= a 100 y > 50, son la cantidad de sueños * los felicidonios
●	En caso contrario, es la división entera de los felicidonios por 2 
-}

cantidadDeSueños :: Persona -> Number
cantidadDeSueños  = length.listaDeSueños


coeficienteDeSatisfaccion ::  Persona  -> Number
coeficienteDeSatisfaccion  persona 
    | (felicidonios persona) > 100 =  felicidonios persona * edad persona
    | (felicidonios persona) <= 100 && (felicidonios persona) > 50 = felicidonios persona * cantidadDeSueños persona
    | otherwise =  (felicidonios persona) `div` 2 
    
{-
Punto b (integrante 2): Grado de ambición de una persona
Saber el grado de ambición de una persona
●	Si los felicidonios son > 100, el grado de ambición son los felicidonios * la cantidad de sueños
●	Si los felicidonios son <= 100 y > 50, será la edad * la cantidad de sueños
●	En caso contrario, serán la cantidad de sueños * 2
-}

gradoDeAmbicion :: Persona -> Number
gradoDeAmbicion persona  
    | (felicidonios persona) > 100 =  felicidonios persona * cantidadDeSueños persona
    | (felicidonios persona) <= 100 && (felicidonios persona) > 50 = edad persona * cantidadDeSueños persona
    | otherwise =  (cantidadDeSueños persona) * 2 


{- ●
¿qué representa la cursiva muy feliz, poco feliz, moderadamente feliz?

RTA :    Representan a las condiciones de los casos de Pruebas.
         Son indicadores que debemos tener en cuenta para modelar nuestras constantes de prueba,
         por ejemplo una persona muy feliz, será una constante llamada personaMuyFeliz
         que representa a un constructor llamado Persona y cuyo valor de felicidonios es mayor a  100. 

¿tiene algún criterio elegir 50, 100 y 101 felicidonios dentro de las pruebas? Justificar.

RTA :    Si, la magnitud es fundamental para verificar que las funciones implementadas evaluén TODOS los posibles escenarios de acuerdo al 
         valor que adquiera el atributo Felicidonio.
         .
-}

{-
Punto 2
ATENCIÓN: Resolver únicamente con Composición y aplicación parcial

No se puede utilizar recursividad ni definir funciones auxiliares en ningún paso de este punto.

Punto a (integrante 1): Nombre largo
Saber si una persona tiene un nombre largo, de más de 10 caracteres.
-}

tieneNombreLargo :: Persona -> Bool
tieneNombreLargo  = ((>10).length).nombre -- Usando aplicación parcial y composición

tieneNombreLargo' :: Persona -> Bool
tieneNombreLargo' persona  = ((>10).length) (nombre persona) -- --Sin usar aplicación parcial

{-
●	¿Qué nombre representa mejor la clase de equivalencia del test: evangelina o personaNombreNormal? Justificar

    RTA: El nombre que utilizamos y que pensamos que representa mejor a la clase de equivalencia del test es personaNombreNormal, 
    ya que podemos modelar una constante del tipo Persona cuyo nombre  se adapta al requerimiento en curso, de esta manera podemos
    abstraernos del nombre propio de la persona y podemos pensarlo de forma más generica y a su vez sumandole versatilidad  ya que incorporamos en 
    en el nombre de la constante la característica que estamos evaluando.
    Podemos mencionar las desventajas de modelar una constante con un nombre propio: es poco representativo,con la primera
    lectura no podremos saber si estamos evaluando una persona , un animal u objeto, ni tampoco podremos saber que característica
    estamos evaluando. Por todas las razones mencionadas anteriormente decidimos elegir como representante de la clase a personaNombreNormal. 
-}

{-
Parte b (integrante 2): Persona suertuda
Saber si una persona es suertuda, que como todos sabemos esto se cumple si su coeficiente de satisfacción es par.
 
●	¿Qué nombre le asignás a la variable para el test de la persona suertuda? ¿Y a la no-suertuda? Justificar.
    
    RTA: Los nombres que utilizamos fueron:  personaConCoeficientePar y personaConCoeficienteImpar  ya que 
    lo que deseamos evaluar  es la paridad del coeficiente de satisfacción de la persona y de acuerdo a su valor
    determinaremos si es una persona con suerte o no.     
-}

personaSuertuda :: Persona -> Bool
personaSuertuda persona = (even.div (coeficienteDeSatisfaccion persona )) 2  --Usando composición

{-
Parte c (integrante 3): Nombre lindo
Saber si una persona tiene un nombre lindo, esto es que su última letra termine en 'a'.
-}

tieneNombreLindo :: Persona -> Bool
tieneNombreLindo   = ((=='a').last).nombre --Usando aplicación parcial y composición,  

tieneNombreLindo' :: Persona -> Bool
tieneNombreLindo'  persona = ((=='a').last) (nombre persona)  --Sin usar aplicación parcial

{-
Señalar:
●	dónde aparece la aplicación parcial
●	dónde la composición
●	y qué ventaja trajo utilizar dichos conceptos. Justificar.

RTA: Para demostrar el uso de la aplicación parcial y la composición decidimos utilizar dos versiones de las funciones propuestas como solución.
     Dejando en evidencia donde aplicamos cada concepto.
    La ventaja que encontramos viene dada en la mejora de la expresividad del código y la simplificación en la escritura de código
    en los módulo de prueba.  

 Aplicación Parcial
 Declaramos el  tipo de dato de  la función tieneNombreLindo para que reciba un solo parámetro del tipo Persona, 
 y del lado derecho de la función utilizamos la funcion nombre  ( que adquirimos luego de haber 
 declarado un  tipo de dato propio Persona) y aplicamos de forma implícita un tipo de dato persona.

 Composición
 Componemos la función nombre con la función resultante de componer last y (=='a'), esta composición recibe un listado de char y 
 si el ultimo caracter es una letra 'a' retornará True.

-}

{-
Punto 3: Los sueños, sueños son...  
Cada persona tiene sueños que cuando los cumple pasan distintas cosas. Modelar los siguientes sueños:
    ●	Integrante 1): Recibirse de una carrera, esto le da 15 felicidonios por cada letra de la carrera 
y le agrega la carrera como habilidad. Ej: "arquitectura" (12letras *15 = 180) le suma 180 felicidonios.
-}

type Suenio = Persona -> Persona
type Carrera = String
type Ciudad = String

recibirseDe :: Carrera -> Suenio 
recibirseDe carrera persona = persona { 
                                      felicidonios = (length carrera) * 15 + felicidonios persona,
                                      listaDeHabilidades = carrera : listaDeHabilidades persona 
                                    }
{-
    ●    Integrante 2): Viajar a una lista de ciudades, suma 100 felicidonios por cada ciudad que visita,
 en el interín pasa un año (la persona tendrá un año más luego de viajar).
 -}                                     
viajarA :: [Ciudad] -> Suenio 
viajarA ciudades persona = persona  { 
                                     felicidonios = length ciudades * 100 + felicidonios persona,
                                     edad = edad persona + 1
                                     }                                     
{-
    ●	Integrante 3): Enamorarse de otra persona, por lo que suma los felicidonios que esta persona tenga.
 El sueño no es bidireccional, que X se enamore de Y no implica lo mismo para Y
-}

enamorarse :: Persona -> Suenio 
enamorarse personaX personaY = personaX { 
                                        felicidonios = felicidonios personaX + felicidonios personaY
                                        }

{-
Común a todos los integrantes, deben implementar también:
●	para los conformistas, el sueño “que todo siga igual”, que mantiene a la persona sin cambios.
●	combo perfecto: se recibe de la carrera de "Medicina", viaja a "Watcher's Hills" (Lomas del Mirador) y "París"
y como bonus extra suma 100 felicidonios por el combo. Definirlo únicamente con funciones existentes.
-}

--queTodoSigaIgual :: Persona -> Persona
queTodoSigaIgual :: Suenio
queTodoSigaIgual persona = persona

--bonusExtra :: Persona -> Persona
bonusExtra :: Suenio
bonusExtra persona =  persona { felicidonios = felicidonios persona + 100 }

--comboPerfecto :: Persona -> Persona
comboPerfecto :: Suenio
comboPerfecto = bonusExtra.viajarA["Watcher's Hills","Paris"].recibirseDe("Medicina")

{-
Punto 4: Queremos modelar distintos tipos de fuentes de los deseos, que se describen a continuación.
Punto a (integrante 1): Fuente minimalista
La fuente minimalista le cumple el primer sueño a la persona, y lo quita de la lista de sueños de esa persona.
-}

--fuenteMinimalista :: Persona -> Persona
fuenteMinimalista :: Suenio
fuenteMinimalista persona = eliminarElPrimerSueño.cumplirElPrimerSueño $ persona

eliminarElPrimerSueño :: Suenio
eliminarElPrimerSueño persona =  persona { listaDeSueños = (drop 1).listaDeSueños $persona}

cumplirEnesimoSueño :: Number -> Suenio
cumplirEnesimoSueño cantidadDeSueños persona =  (listaDeSueños persona) !! (cantidadDeSueños-1) $persona

cumplirElPrimerSueño :: Suenio
cumplirElPrimerSueño  = cumplirEnesimoSueño 1

{-
Punto b (integrante 2): Fuente copada
La fuente copada le cumple todos los sueños a la persona. La persona debe quedarse sin sueños.
-}

{-
fuenteCopada :: Suenio
fuenteCopada persona
    | (length . listaDeSueños) persona == 0    = persona
    | otherwise    = (fuenteCopada . fuenteMinimalista) persona
-}

{-
En este caso lo que queremos es dada una persona, cumplir todos sus sueños y luego quitarlos. Esto nos dice que la lista que recibe el fold debería ser la de sueños:
fuenteCopada persona = foldl funciónAcumuladora? valorInicial? . listaDeSuenios $ persona
Luego, el valor inicial sale fácil, debería ser la persona a la que queremos cumplir los sueños (sería nuestro punto de partida)
fuenteCopada persona = foldl funciónAcumuladora? persona . listaDeSuenios $ persona
Nos queda la función para acumular el resultado.
Acá tenemos dos opciones:
Hacer una función auxiliar cumplirSuenio que recibe una persona y un sueño, aplica el sueño y lo quita de la lista de sueño. En este caso nos quedaría: fuenteCopada persona = foldl cumplirSuenio persona . listaDeSuenios $ persona
Resolver en el fold sólo la parte de aplicar los sueños y luego sacarlos. Nos quedaría: fuenteCopada persona = sacarSuenios . foldl (\persona suenio -> suenio persona) persona . listaDeSueños $ persona . En este caso les quedaría implementar la función sacarSuenios
Hay una tercera opción que es más una variación de la segunda. Si ven la función lambda que se le pasa recibe 2 parámetros, un valor y una función y aplica la función al valor
es muy parecido a lo que hace la función $ sólo que tiene los parámetros al revés (el suenio debería ser el primero). Para resolver eso se puede usar foldr :
fuenteCopada persona = sacarSuenios . foldr ($) persona . listaDeSueños $ persona
-}


--2daVersion

sacarSueños :: Persona -> Persona
sacarSueños persona = persona { listaDeSueños = []    }

fuenteCopada :: Suenio
fuenteCopada persona = sacarSueños . foldl (\persona suenio -> suenio persona) persona . listaDeSueños $ persona

--en la funcion lambda defino la función que estará iterando por la lista de sueños, y a su vez cumpliendolos.

{-
Punto c (integrante 3): Fuente a pedido
La fuente, a pedido, le cumple el enésimo sueño a una persona, pero no lo quita de la lista de sueños.
-}

fuenteApedido :: Number -> Suenio
fuenteApedido nroSueño = cumplirEnesimoSueño nroSueño 

{-
Punto d) (todos los integrantes)
Modelar la fuente sorda: como no entiende bien qué sueño tiene que cumplir, no le cumple ninguno.

Para pensar (todos los integrantes):
¿cómo modelaron cada una de las fuentes? ¿por qué?
--RTA: Las fuentes fueron modeladas como funciones con el objetivo de lograr el mayor acoplamiento posible y  evitar la repetición de código de forma innecesaria.
En primero lugar, observamos el comportamiento de cada una de las fuentes y notamos que existían ciertas funcionalidades que podrían llegar a reutilizarse,
por ello es que empezamos por realizar la función auxiliar llamada cumplirEnesimoSueño que es el núcleo de la función fuenteApedido y luego creamos 
la función llamada cumplirElPrimerSueño que consiste en recibir como parametro un 1 y luego utilizarla en la composición junto con la función eliminarElPrimerSueño
de fuenteMinimalista.  
-}

fuenteSorda :: Suenio
fuenteSorda = queTodoSigaIgual

{-
Punto 5
Dada una lista de fuentes y una persona, saber cuál es la fuente "ganadora" en base a un criterio.
Por ejemplo:
el que más felicidonios le de a esa persona cuando lo cumpla (integrante 1)
-}


criterioMayorFelicidonios:: [Suenio] -> Persona -> Suenio
criterioMayorFelicidonios  = mejorFuente comparacionDeFuentesPorMayorFelicidonios 


mejorFuente  :: (Suenio -> Suenio -> Persona ->Bool)->[Suenio] -> Persona -> Suenio
mejorFuente funcionDeComparacionDeFuentes [unaFuente] persona = unaFuente
mejorFuente funcionDeComparacionDeFuentes (unaFuente:dosFuentes:colaDeFuentes) persona 
    | funcionDeComparacionDeFuentes unaFuente dosFuentes persona = mejorFuente funcionDeComparacionDeFuentes (unaFuente : colaDeFuentes) persona 
    | otherwise =  mejorFuente funcionDeComparacionDeFuentes (dosFuentes:colaDeFuentes) persona


comparacionDeFuentesPorMayorFelicidonios :: Suenio -> Suenio -> Persona -> Bool
comparacionDeFuentesPorMayorFelicidonios unaFuente dosFuentes persona = (felicidonios.unaFuente) persona > (felicidonios.dosFuentes) persona 


{-
el que menos felicidonios le de a esa persona cuando lo cumpla (integrante 2)
-}

criterioMenorFelicidonios :: [Suenio] -> Persona -> Suenio
criterioMenorFelicidonios =  mejorFuente comparacionDeFuentesPorMenorFelicidonios


comparacionDeFuentesPorMenorFelicidonios :: Suenio -> Suenio -> Persona -> Bool 
comparacionDeFuentesPorMenorFelicidonios unaFuente dosFuentes persona = (felicidonios.unaFuente) persona < (felicidonios.dosFuentes) persona 

{-
el que más habilidades le deje a esa persona cuando lo cumpla (integrante 3)
Cada integrante debe contar cómo invocar a esa función desde la consola para resolver ese requerimiento. Se puede usar recursividad
-}


criterioMayorCantidadHabilidades  :: [Suenio] -> Persona -> Suenio
criterioMayorCantidadHabilidades  = mejorFuente comparacionDeFuentesPorMayorHabilidades

comparacionDeFuentesPorMayorHabilidades :: Suenio -> Suenio -> Persona -> Bool
comparacionDeFuentesPorMayorHabilidades unaFuente dosFuentes persona = (length.listaDeHabilidades.unaFuente) persona > (length.listaDeHabilidades.dosFuentes) persona


{-Cada integrante debe contar cómo invocar a esa función desde la consola para resolver ese requerimiento.-}

{-
PtoA)
*Spec> criterioMayorFelicidonios [recibirseDe "maestra", recibirseDe "abogada"] evangelina 
<una función>

PtoB)
*Spec> criterioMayorFelicidonios [recibirseDe "maestra", recibirseDe "abogada"] evangelina 
<una función>

PtoC) 
*Spec> criterioMayorCantidadHabilidades dosSueños evangelina 
<una función>
-}

{-

Punto 6: Reportes
Se necesita implementar los siguientes requerimientos

Integrante 1: Saber qué sueños son valiosos para una persona, son aquellos que al cumplirlos la persona queda con más de 100 felicidonios.
-}

felicidoniosSueñoCumplido :: Persona -> Suenio -> Number
felicidoniosSueñoCumplido persona sueño = (felicidonios.sueño) persona

sueñosValiosos :: Persona -> [Suenio]
sueñosValiosos persona = filter ((>100).felicidoniosSueñoCumplido persona) $listaDeSueños persona

{- 
Integrante 2: Saber si algún sueño de una persona es raro, que es el que lo deja  con la misma cantidad de felicidonios tras cumplirlo.
-}

tieneAlgunSueñoRaro  :: Persona -> Bool
tieneAlgunSueñoRaro persona = any ((== felicidonios persona) . (felicidoniosSueñoCumplido persona))  $listaDeSueños persona

{-
Integrante 3: Dada una lista de personas, poder conocer la felicidad total de ese
grupo si cumplen todos sus sueños. Resolverlo con fold  (left o right, la que más le guste).
-}

--felicidadDelGrupo :: [Persona] -> Number
--felicidadDelGrupo grupo = foldr ((+) . flip (felicidoniosSueñoCumplido) fuenteCopada) 0 grupo
--felicidadDelGrupo grupo = foldl (\totalFelicidonios persona -> totalFelicidonios + felicidoniosSueñoCumplido persona fuenteCopada') 0 grupo

{-
Para pensar (todos los integrantes):
6i) ¿Dónde aparecen los conceptos de aplicación parcial y orden superior? Justifique.

Rta: 
6ia) El  concepto de aplicación parcial aparece en la composición que está en la primer componente del filter, donde la función felicidoniosSueñoCumplido utiliza solamente un parámetro (persona) y el otro
parámetro que espera (un sueño) queda definido de forma implicita.
Se ve la aplicación del concepto de orden superior  cuando usamos una de las funciones típicas de su reportorio, tal como lo es la función filter, que para realizar su propósito 
(seleccionar los sueños que hacen ganar a la persona mas de 100 felicidonios) hace uso de la función  felicidoniosSueñoCumplido.

6ib) El  concepto de aplicación parcial aparece en la composición que está en la primer componente del any, donde la función felicidoniosSueñoCumplido utiliza solamente un parámetro (persona) y el otro
parámetro que espera (un sueño) queda definido de forma implicita. 
Se ve la aplicación del concepto de orden superior  cuando usamos una de las funciones típicas de su reportorio, tal como lo es la funcion any, que para realizar su propósito  (saber si algun sueño de la persona es raro)
hace uso de otra  función  como lo es felicidoniosSueñoCumplido.

6ii) ¿Cómo se relacionan las soluciones de los puntos 5 y el 6 respecto al concepto de declaratividad? Justificar.

Rta: El concepto de declaratividad permite un  mayor grado de reutilización y de abstracción en tareas repetitivas, prioriza el "qué" estamos haciendo y no en "cómo" lo  estamos haciendo que sería el enfoque o las bases del
paradigma imperativo. Esto quiere decir que nosotros expresamos nuestra lógica sin describir controles de flujo (no empleamos ciclos o estructuras condicionales) con el objetivo de tener un código con menor cantidad de lineas y
a su vez mucho mas legible en comparación a realizar la misma funcionalidad bajo un paradigma imperativo.
Las soluciones de los puntos 5 y 6 se relacionan con el concepto definido anteriormente de la siguiente manera: en el punto 5 la solución encontrada no es del todo recomendada bajo este paradigma por el hecho de hacer
uso de la recursión, pero a su vez se visualiza una ventaja del paradigma que es el poder retornar funciones como conjunto imagen de la función solución, en el punto 6 a pesar de que hicimos  uso de una función auxiliar
intentamos explotar al máximo el concepto de declaratividad y orden superior, tratando de lograr una implementación que sea lo mas "limpia" y legible posible. 
-}

{-
Tip: aprovecharse de alguna de las fuentes definidas anteriormente.
Cada integrante será responsable de generar los casos de prueba.

Resolver el punto invocando  únicamente las funciones de orden superior y aplicación parcial
Para pensar (todos los integrantes):
¿Dónde aparecen los conceptos de aplicación parcial y orden superior? Justifique.
¿Cómo se relacionan las soluciones de los puntos 5 y el 6 respecto al concepto de declaratividad? Justificar.
-}

{-
Punto 7
Modelar a una persona con sueños infinitos  (una para todos los integrantes).
Para cada integrante, teniendo en cuenta el requerimiento que le tocó al modelar la fuente en el punto 4,
¿es posible que la fuente pueda satisfacer (es decir, cumplir el/los deseos) a esa persona que tiene infinitos sueños?
Justificar su respuesta con un ejemplo concreto: “a esta persona P0 con infinitos sueǹos S0  y la Fuente F1 la invoco en la consola y... (etc. etc. etc.)”
y relacionarlo con algún  concepto visto en la cursada.
-}
{-
Rta:

7a)
Para responder a este punto, modelamos a una persona cuyo lista de Sueños es infinita ya que 
    definimos un sueño recursivo, donde esta entidad se enamora de si misma, se puede ver en la linea
    381 , enamorarse raritoTres y la entidad que usaremos para responder a este punto lo llamaremos raritoTres.

raritoTres = Persona { edad = 28,
                       nombre = "Rarito",
                       listaDeSueños = [queTodoSigaIgual,enamorarse raritoTres],
                       felicidonios = 10,
                       listaDeHabilidades = []
                     }
-

Recordando la  definición de  fuenteMinimalista 
fuenteMinimalista persona = eliminarElPrimerSueño.cumplirElPrimerSueño $ persona

En el punto 7a : 
 “A la  persona raritoTres cuyos sueños son infinitos y la fuenteMinimalista, la invoco en la consola de la siguiente manera:
 *Spec Library Spec> fuenteMinimalista raritoTres
  Bajo el concepto de evaluación diferida o perezosa propia del lenguaje del Haskell, el compilador solo evaluará la parte que necesita,
  en este caso será la cabeza de la lista y es por ello que será posible obtener el siguiente resultado por consola , que será propio 
  de cumplirle el primer sueño a raritoTres y a su vez elimarlo de su lista de Sueños.”
 
 *Spec Library Spec> fuenteMinimalista raritoTres
 Persona {edad = 28, listaDeSueños = [<una función>], nombre = "Rarito", felicidonios = 10, listaDeHabilidades = []}


En el punto 7b : 
---NOTA : Rta en base a la  resolución de fuenteCopada mediante el uso de recursividad--

 “A la  persona raritoTres cuyos sueños son infinitos y la fuenteCopada que se encargar de cumplir todos los sueños de una persona,
 la invoco en la consola de la siguiente manera:
 *Spec Library Spec> fuenteCopada raritoTres
 El interprete no podrá devolver ningun resultado, ya que la fuenteCopada intentará cumplir todos los sueños de raritoTres y 
 la recursividad no tendrá un punto de corte.

En el punto 7c : 
 “A la  persona raritoTres cuyos sueños son infinitos y la fuenteApedido que se encargar de cumplir un sueño X de la  persona,
 la invoco en la consola de la siguiente manera:
 *Spec Library Spec> fuenteApedido 2 raritoTres

 Por lo dicho en el punto 7a) Bajo el concepto de evaluación diferida propia del lenguaje, el compilador
 solo trabajará con la parte que necesita, para esta función  solo espera un  índice que será el indicador del sueño a cumplir.
 Es por ello que obtendremos el siguiente resultado:

*Spec Library Spec> fuenteApedido 2 raritoTres
Persona {edad = 28, listaDeSueños = [<una función>,<una función>], nombre = "Rarito", felicidonios = 20, listaDeHabilidades = []}


-}
