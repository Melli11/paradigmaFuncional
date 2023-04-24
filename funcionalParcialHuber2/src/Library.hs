module Library where
import PdePreludat


{-
Una agencia de remises contrata los más eficientes choferes de los que conoce:
●	el nombre
●	el kilometraje de su auto
●	los viajes que tomó
●	qué condición impone para tomar un viaje

Cada viaje se hace en una fecha particular, lo toma un cliente (queremos saber su nombre y dónde vive) y tiene un costo.

En cuanto a la condición para tomar un viaje
●	algunos choferes toman cualquier viaje
●	otros solo toman los viajes que salgan más de $ 200
●	otros toman aquellos en los que el nombre del cliente tenga más de n letras
●	y por último algunos requieren que el cliente no viva en una zona determinada
-}

{-
Se pide
1.	(2 puntos)
Modelar los TAD cliente, chofer y viaje.
-}
type Domicilio = String
type Nombre = String
type Cliente = (Nombre, Domicilio)

data Viaje = Viaje {
    fecha :: (Number,Number,Number),
    cliente :: Cliente,
    costo :: Number
} deriving (Show)

data Chofer = Chofer {
    nombre :: Nombre,
    kilometraje :: Number,
    viajes :: [Viaje],
    condicionViaje :: Viaje -> Bool
} deriving (Show)

{-
2.	(2 puntos) 
Implementar con las abstracciones que crea conveniente las condiciones que cada chofer tiene para tomar un viaje. 
Debe utilizar en este punto composición y aplicación parcial.

-}

cualquierViaje ::  (Viaje -> Bool) 
cualquierViaje  _ = True

--viajeMayorADoscientos :: Viaje -> Bool
--viajeMayorADoscientos  viaje = costo viaje > 200 

viajeMayorADoscientos :: (Viaje -> Bool)
viajeMayorADoscientos  = (>200).costo 

viajeSegunNombre :: Number -> (Viaje -> Bool)
viajeSegunNombre n  =  (>n).length.fst.cliente  

noViajaAlaZonaDe :: Domicilio -> (Viaje -> Bool)
noViajaAlaZonaDe zona = (zona /= ).snd.cliente

{-
3.	(1 punto) Definir las siguientes expresiones: 
a.	el cliente “Lucas” que vive en Victoria
b.	el chofer “Daniel”, su auto tiene 23.500 kms., hizo un viaje con el cliente Lucas el 20/04/2017 
    cuyo costo fue $ 150, y toma los viajes donde el cliente no viva en “Olivos”.
c.	la chofer “Alejandra”, su auto tiene 180.000 kms, no hizo viajes y toma cualquier viaje.
-}

viajeEjemplo :: Viaje
viajeEjemplo = Viaje (20, 4, 2017) clienteLucas 150

viajeAOlivos :: Viaje
viajeAOlivos = Viaje (20, 4, 2017) clienteFalopa 150

clienteFalopa :: Cliente 
clienteFalopa = ("Fantasma","Olivos")

clienteLucas :: Cliente
clienteLucas = ("Lucas","Victoria")

daniel :: Chofer
daniel = Chofer "Daniel" 23500 [Viaje (20, 4, 2017) clienteLucas 150 ] (noViajaAlaZonaDe "Olivos")

alejandra  :: Chofer
alejandra = Chofer "Alejandra" 180000  [] cualquierViaje 

{-
4.	(1 punto)
Saber si un chofer puede tomar un viaje.
-}

-- Punto 4

puedeTomarElViaje :: Viaje -> Chofer -> Bool
puedeTomarElViaje  viaje  chofer = (condicionViaje chofer) viaje 

{-
Probar en consola
puedeTomarElViaje (head.viajes $daniel) daniel

o definiendo un viaje de ejemplo

puedeTomarElViaje viajeEjemplo daniel

-}


{-
5.	(2 puntos) 
Saber la liquidación de un chofer, que consiste en sumar los costos de cada uno de los viajes. Por ejemplo, Alejandra tiene $ 0 y Daniel tiene $ 150.
-}

liquidacionDeUnChofer :: Chofer -> Number
liquidacionDeUnChofer = sum.map costo.viajes

{- ORDEN SUPERIOR

6.	(4 puntos)
Realizar un viaje: dado un viaje y una lista de choferes, se pide que
a.	filtre los choferes que toman ese viaje. Si ningún chofer está interesado, no se preocupen: el viaje no se puede realizar.
-}

realizarUnViaje :: Viaje -> [Chofer] -> [Chofer]
realizarUnViaje viaje = filter $puedeTomarElViaje viaje

{-
6b.	considerar el chofer que menos viaje tenga. Si hay más de un chofer elegir cualquiera.
-}

choferConMenosViajes :: [Chofer] -> Chofer
choferConMenosViajes [chofer] = chofer --si hay un solo chofer devuelve ese chofer
choferConMenosViajes (chofer1:chofer2:choferes) = choferConMenosViajes ((tieneMenosViaje chofer1 chofer2):choferes) --utiliza una estructura recursiva para agregar los choferes ordenados con menor cantidad de viajes 

tieneMenosViaje :: Chofer -> Chofer -> Chofer --compara dos choferes y retorna el que con menos viajes tiene
tieneMenosViaje chofer1 chofer2 
    | (length . viajes ) chofer1 > (length . viajes ) chofer2 = chofer2 
    | otherwise = chofer1

{-
6c.	efectuar el viaje: esto debe incorporar el viaje a la lista de viajes del chofer. ¿Cómo logra representar este cambio de estado?
-}

efecturaViaje :: Chofer -> Viaje -> Chofer
efecturaViaje chofer viaje = chofer { viajes = viaje : viajes chofer}

{-
7.	(1 punto) Al infinito y más allá
a.	Modelar al chofer “Nito Infy”, su auto tiene 70.000 kms., que el 11/03/2017 hizo infinitos viajes de $ 50 con Lucas
y toma cualquier viaje donde el cliente tenga al menos 3 letras. Puede ayudarse con esta función:
repetirViaje viaje = viaje : repetirViaje viaje
-}
nitoInfy :: Chofer
nitoInfy = Chofer { nombre = "Nito Infy",
                    kilometraje = 70000,
                    viajes = viajeInfinito,
                    condicionViaje = viajeSegunNombre 3
                  }
--nitoInfy = Chofer "Nito Infy" 70000  viajeInfinito (viajeSegunNombre 3) 

viajeInfinito = repetirViaje viajeNitoInfy

viajeNitoInfy = Viaje { fecha = (11,03,2017) ,
                        cliente = clienteLucas , 
                        costo = 50
                      }

--viajeNitoInfy = Viaje (11,03,2017) clienteLucas 50

repetirViaje :: Viaje -> [Viaje]
repetirViaje viaje = viaje : repetirViaje viaje


{-
b.	¿Puede calcular la liquidación de Nito? Justifique.
-}

-- Rta = No es posible calcular el costo ya que el chofer posee viajes infinitos, y la función 
-- para hallar la liquidación llamada liquidacionDeUnChofer accede al campo costos de los viajes 
-- para realizar el calculo, es decir que la lista de viaje no posee una dimensión calculable,
--  por lo tanto al no existir el punto de corte, el motor no podrá realizar el cálculo. 

{-
c.	¿Y saber si Nito puede tomar un viaje de Lucas de $ 500 el 2/5/2017? Justifique. 
-}

nitoInfyC :: Chofer
nitoInfyC = Chofer { nombre = "Nito Infy",
                    kilometraje = 70000,
                    viajes = [viajeDeLucas],
                    condicionViaje = viajeSegunNombre 3
                  }
--nitoInfy = Chofer "Nito Infy" 70000  viajeInfinito (viajeSegunNombre 3) 

viajeDeLucas = viajeNitoInfyC

viajeNitoInfyC = Viaje { fecha = (2,5,2017) ,
                        cliente = clienteLucas , 
                        costo = 500
                      }


{-
8.	(1 punto) Inferir el tipo de la función gōngnéng

gongNeng arg1 arg2 arg3 = 
     max arg1 . head . filter arg2 . map arg3
-}



