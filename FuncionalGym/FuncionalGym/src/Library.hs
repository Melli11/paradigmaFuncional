module Library where
import PdePreludat

{-
Se desea desarrollar un sistema para un popular gimnasio que permita calcular el efecto de las rutinas de ejercicios
 que realizan sus socios.
De cada gimnasta nos interesa saber su peso y su coeficiente de tonificación.
Los profesionales del gimnasio preparan rutinas de ejercicios pensadas para las necesidades de cada gimnasta.
Una rutina es una lista de ejercicios que el gimnasta realiza durante unos minutos para quemar calorías y tonificar sus músculos.
Se pide:
	1. Modelar a los Gimnastas y las operaciones necesarias para hacerlos ganar tonificación y 
    quemar calorías considerando que por cada 500 calorías quemadas se baja 1 kg de peso.
-}

data Gimnasta = Gimnasta {
        peso :: Num,
        tonificacion :: Num
}

tonificar :: Num-> Gimnasta -> Gimnasta
tonificar numTonificacion gimnasta = gimnasta { tonificacion = tonificacion gimnasta + numTonificacion }

quemarCalorias :: Num-> Gimnasta -> Gimnasta
quemarCalorias  calorias gimnasta = gimnasta { peso = peso gimnasta - calorias `div` 500 } 

data Rutina = Rutina {
        nombre :: String,
        duracionTotal :: Num,
        ejercicios :: [Ejercicio]
}deriving (Show)
{-
	 Modelar los siguientes ejercicios del gimnasio:
	
    2. La cinta es una de las máquinas más populares entre los socios que quieren perder peso. 
    Los gimnastas simplemente corren sobre la cinta y queman calorías en función de la velocidad
     promedio alcanzada (quemando 10 calorías por la velocidad promedio por minuto).
-}
    --cinta :: Int -> Int -> Gimnasta -> Gimnasta  podriamos haber hecho esta funcion que recibe 3 parametros
    -- retorna un gimnasta modificado. En su lugar aprovecharemos la función quemar calorias.
cinta :: Num -> Num -> (Gimnasta -> Gimnasta)
cinta velocidad tiempo = quemarCalorias (velocidad * 10 * tiempo)
{-
    La cinta puede utilizarse para realizar dos ejercicios diferentes:
				2.a La caminata es un ejercicio en cinta con velocidad constante de 5 km/h.
-}
caminata :: Num -> Gimnasta -> Gimnasta
caminata tiempo = cinta 5 tiempo
caminata'  = cinta 5 --con notación point free

{-
    2.b.El pique arranca en 20 km/h y cada minuto incrementa la velocidad en 1 km/h,
    con lo cual la velocidad promedio depende de los minutos de entrenamiento.
-}
pique :: Num ->  Gimnasta -> Gimnasta
pique tiempo = cinta (tiempo `div` 2 + 20)  tiempo --en este caso la velocidad depende del tiempo por ello hay que explicitarlo como parametro

{-
    2. Las pesas son el equipo preferido de los que no quieren perder peso, sino ganar musculatura.
    Una sesión de levantamiento de pesas de más de 10 minutos hace que el gimnasta gane una tonificación
    equivalente a los kilos levantados. 
    Por otro lado, una sesión de menos de 10 minutos es demasiado corta, y no causa ningún efecto en el gimnasta.
-}

levantarPesas :: Num -> Num -> Gimnasta -> Gimnasta
levantarPesas tiempo peso gimnasta  | tiempo > 10 = tonificar peso gimnasta
                                    | otherwise = gimnasta

{- Usando aplicación parcial

    levantarPesas' :: Num -> Num -> (Gimnasta -> Gimnasta)
    levantarPesas' tiempo peso   | tiempo > 10 = tonificar peso 
                                        | otherwise = id
-}
{-
	2c. La colina es un ejercicio que consiste en ascender y descender sobre una superficie inclinada y 
    quema 2 calorías por minuto multiplicado por la inclinación con la que se haya montado la superficie.

    Los gimnastas más experimentados suelen preferir otra versión de este ejercicio: 
    la montaña, que consiste en 2 colinas sucesivas (asignando a cada una la mitad del tiempo total),
    donde la segunda colina se configura con una inclinación de 5 grados más que la inclinación de la primera.
    Además de la pérdida de peso por las calorías quemadas en las colinas, la montaña incrementa en 3 unidades 
    la tonificación del gimnasta.
-}

colina :: Num -> Num -> (Gimnasta -> Gimnasta)
colina tiempo inclinacion = quemarCalorias ( 2 * tiempo * inclinacion)

montaña :: Num -> Num -> (Gimnasta -> Gimnasta)
montaña tiempo inclinacion = tonificar 3.colina (tiempo `div` 2) (inclinacion + 5 ). colina (tiempo `div` 2) (inclinacion)

{-
	Implementar una función realizarRutina, que dada una rutina y un gimnasta retorna el gimnasta resultante 
    de realizar todos los ejercicios de la rutina, repartiendo el tiempo total de la rutina en partes iguales.
    Mostrar un ejemplo de uso con una rutina que incluya todos los ejercicios del punto anterior.
-}	
 realizarRutina ::  Rutina -> Gimnasta -> Gimnasta


{-
data Rutina = Rutina {
        nombre :: String,
        duracionTotal :: Num,
        ejercicios :: [Ejercicio]
}deriving (Show)

-}
 --funcion fold
 --realizarRutina (Rutina nombre duracion ejercicios) gimnasta = 
 {-   4. Definir las operaciones necesarias para hacer las siguientes consultas a partir de una lista de rutinas:
		1. ¿Qué cantidad de ejercicios tiene la rutina con más ejercicios?
		2. ¿Cuáles son los nombres de las rutinas que hacen que un gimnasta dado gane tonificación?
    ¿Hay alguna rutina peligrosa para cierto gimnasta? Decimos que una rutina es peligrosa para alguien si 
    lo hace perder más de la mitad de su peso
-}