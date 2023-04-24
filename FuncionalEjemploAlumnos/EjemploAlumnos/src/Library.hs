module Library where
import PdePreludat
--import Text.Show.Functions

{-
5.1 Alumnos
Modelar un alumno, que define
● un nombre,
● la fecha de nacimiento,
● el legajo (sin dígito verificador),
● las materias que cursa
● y el criterio para estudiar ante un parcial:
○ algunos son estudiosos: estudian siempre,
○ otros son hijos del rigor: estudian si el parcial tiene más de n
preguntas,
○ y también están los cabuleros, que estudian si la materia tiene una
cantidad impar de letras.
-}


{-
5.2 Requerimientos
1. Modelar un parcial
2. Modelar el tipo que representa el criterio de estudio.
3. Modelar genéricamente un alumno.
4. Representar con la abstracción que crea más conveniente al criterio
estudioso, hijo del rigor y cabulero.
5. Modelar a Nico, un alumno estudioso
6. Hacer que Nico pase de ser estudioso a hijo del rigor (buscar una
abstracción lo suficientemente genérica)
7. Determinar si Nico va a estudiar para el parcial de Paradigmas
-}

{-

5.3 Modelar un parcial
Para resolver este requerimiento, tenemos que abstraer la información
necesaria para poder resolver lo que hoy nos piden. Ciertamente, hay mucha
información que un parcial puede tener:
● hora de comienzo
● hora de fin
● el profesor que lo toma
● la materia
● el aula
● la cantidad de alumnos presentes
● la cantidad de preguntas
● las preguntas propiamente dichas...
... entre otros datos. 
Pero en este enunciado, solamente nos importan dos cosas:
la materia (para los cabuleros) y la cantidad de preguntas que tiene un
parcial (para los hijos del rigor).
Esta técnica que empleamos toma en cuenta lo que nos interesa y descarta lo
que no es esencial para la solución, proceso que se llama abstracción .
-}
{-
● ¿Cómo modelamos la materia? No necesitamos que sea una estructura
compuesta, nos alcanza con que sea un String.
● Respecto a las preguntas, tampoco necesitamos conocer la lista de
preguntas concreta, nos basta con saber la cantidad por el momento,
entonces un entero es suficiente.
-}

--Solucion Oficial
{-
data Parcial = Parcial String Number deriving ( Show )
materia :: Parcial -> String
materia ( Parcial mat _ ) = mat
cantidadPreguntas :: Parcial -> Number
cantidadPreguntas ( Parcial _ cant) = cant
-}
--Usando Record Syntax

data Parcial = Parcial {
                materia :: String,
                cantidadPreguntas :: Number
                } deriving (Show)

{-
5.4 Modelar el tipo del criterio de estudio
¿Qué representa el criterio? Dado un parcial, queremos saber si va a estudiar. Es
decir: Parcial -> Bool
Exacto, el criterio se modela con una función, que va a formar parte de la
estructura de un alumno (Del DATA). Por eso no queremos usar:
● códigos numéricos (1 = estudia siempre, 2 = hijo del rigor, 3 = cabulero)
● ni códigos alfabéticos (“ES”, “HR”, “CA”)
● ni ningún otro mecanismo de indirección: en la estructura del alumno
debe estar el criterio que nos diga si va a estudiar ante un parcial.
Para definir el tipo, escribimos:
-}

type CriterioEstudio = Parcial -> Bool --Por Sintaxis el tipo de dato empieza siempre con Mayuscula

{-
5.5 Modelar un alumno genérico
Un alumno se puede representar como una estructura
● con tuplas
● o bien con un tipo de dato propio ( data )
Dada la gran cantidad de información que se asocia a un alumno, vamos a elegir
definir un tipo de dato propio con notación de registro ( record syntax ).
La siguiente decisión es analizar de qué tipo es cada uno de los datos del
alumno:
-}

data Alumno = Alumno{
    nombre :: String,
    fechaNacimiento :: (Number,Number,Number),
    legajo :: Number,
    materiasQueCursa :: [String],
    criterioEstudio :: CriterioEstudio
} deriving (Show)

{-
5.6 Representar los criterios de estudio enunciados
Definimos entonces tres funciones:
El estudioso que siempre estudia (no importa el parcial):
-}

estudioso :: CriterioEstudio --Siempre estudia
estudioso _ = True
estudioso' :: Parcial -> Bool
estudioso' _ = True
{-
El hijo del rigor, al que le podemos configurar la cantidad de preguntas por
sobre las cuales comienza a estudiar: -}
hijoDelRigor :: Number -> CriterioEstudio
hijoDelRigor n (Parcial _ cantidadPreguntas) = cantidadPreguntas > n

{-
Cabuleros:estudian si la materia tiene una cantidad impar de letras
-}
cabulero :: CriterioEstudio
cabulero' :: CriterioEstudio
cabulero' (Parcial materia _)= (not.even.length) materia
cabulero (Parcial materia _)= (odd.length) materia

{-
5.7 Modelar a Nico, un alumno estudioso
Aprovechamos la definición anterior de estudioso para escribir a la expresión
que describe a Nico:
-}

nico = Alumno {
    nombre = "Nico",
    fechaNacimiento = (11,08,90),
    legajo = 1410000,
    materiasQueCursa = ["AM","PdP","AdS"],
    criterioEstudio = estudioso

}
-- la mas optima , solo valido con record syntax 
--Esta opción permite generar un nuevo alumno indicando sólo lo que cambia:
{-
cambiarCriterioDeEstudio nuevoCriterio alumno = alumno {
        criterioEstudio = nuevoCriterio
-}

-- la mas expresiva
cambiarCriterioDeEstudio nuevoCriterio alumno = Alumno {
        nombre = nombre alumno,
        fechaNacimiento = fechaNacimiento alumno,
        legajo = legajo alumno,
        materiasQueCursa = materiasQueCursa alumno,
        criterioEstudio = nuevoCriterio 
} 

{-
5.9 Saber si un alumno va a estudiar para el parcial de
Paradigmas Ok, necesitamos
● un alumno: podemos usar Nico
● un parcial de Paradigmas
Si estudia o no dependerá del criterio del alumno. Entonces:
-}

estudiaraParaElParcial  parcial alumno = (criterioEstudio alumno) parcial 