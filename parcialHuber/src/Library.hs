module Library where
import PdePreludat
-- import Data.List (sortBy, head)

data Chofer = Chofer{
    nombre :: String,
    kilometros :: Number,
    viajesRealizados :: [Viaje],
    condicionDeViaje :: CondicionViaje
} deriving (Show)

data Viaje = Viaje {
    fechaDeViaje :: (Dia,Mes,Anio),
    cliente :: Cliente,
    costo :: Number
} deriving (Show)

type Dia = Number
type Mes = Number
type Anio = Number
type Nombre = String
type Domicilio = String
type Cliente = (Nombre,Domicilio)

-- Funciones Auxiliares

nombreDelCliente :: Viaje -> Nombre
nombreDelCliente = fst.cliente

domicilioDelCliente :: Viaje -> Nombre
domicilioDelCliente = snd.cliente

type CondicionViaje = Viaje -> Bool

aceptaCualquierViaje :: CondicionViaje
aceptaCualquierViaje _ = True
aceptaLosViajesMayoresA200 :: CondicionViaje
aceptaLosViajesMayoresA200  =  (>200).costo
aceptaViajeDeAcuerdoAlNombreDelCli :: Number -> CondicionViaje
aceptaViajeDeAcuerdoAlNombreDelCli cantidadLetras = (cantidadLetras <= ).length . nombreDelCliente

aceptaViajeSegunZona' :: Domicilio -> CondicionViaje
aceptaViajeSegunZona' zona viaje =    zona  /=  domicilioDelCliente viaje

aceptaViajesQueNOseanDe :: Domicilio -> CondicionViaje
aceptaViajesQueNOseanDe zona =   (/= zona)  .   domicilioDelCliente

-- (1 punto) Definir las siguientes expresiones:
-- a. el cliente “Lucas” que vive en Victoria

clienteLucas :: Cliente
clienteLucas = ("Lucas","Victoria")

clienteZonaComplicada :: Cliente
clienteZonaComplicada = ("Melli","Olivos")

-- b. el chofer “Daniel”, su auto tiene 23.500 kms., hizo un viaje con el cliente
-- Lucas el 20/04/2017 cuyo costo fue $ 150, y toma los viajes donde el
-- cliente no viva en “Olivos”.

choferDaniel :: Chofer
choferDaniel = Chofer "Daniel" 23500 [Viaje (20,04,2017) clienteLucas 150] (aceptaViajesQueNOseanDe "Olivos")

-- c. la chofer “Alejandra”, su auto tiene 180.000 kms, no hizo viajes y toma
-- cualquier viaje.

choferAlejandra :: Chofer
choferAlejandra = Chofer "Alejandra" 180000 [] aceptaCualquierViaje

-- 4(1 punto) Saber si un chofer puede tomar un viaje.

viajeDeEjemplo :: Viaje
viajeDeEjemplo = Viaje (20,04,2017) clienteLucas 150


-- puedeTomarViajeUnChofer ::  (Viaje -> Bool) -> Chofer 
puedeTomarViajeUnChofer :: Viaje -> Chofer -> Bool
puedeTomarViajeUnChofer  unViaje unChofer = condicionDeViaje unChofer unViaje

-- En consola 
-- *Spec Library Spec> puedeTomarViajeUnChofer  viajeDeEjemplo  choferDaniel 

-- 5. (2 puntos) Saber la liquidación de un chofer, que consiste en sumar los costos
-- de cada uno de los viajes. Por ejemplo, Alejandra tiene $ 0 y Daniel tiene $
-- 150.

liquidacionDeUnChofer :: Chofer -> Number
liquidacionDeUnChofer unChofer =   sum.map costo $viajesRealizados unChofer


-- 6. (4 puntos) Realizar un viaje: dado un viaje y una lista de choferes, se pide que
-- se le asigne el viaje al chofer que: lo pueda aceptar, es decir que el viaje cumpla con sus requisitos,
-- ademas el chofer debe ser el que menos viajes hizo de entre todos los choferes disponibles, y a la vez 
-- incorporarle el viaje al chofer.


-- a. filtre los choferes que toman ese viaje. Si ningún chofer está
-- interesado, no se preocupen: el viaje no se puede realizar.

realizarViaje :: Viaje -> [Chofer] -> Chofer
realizarViaje unViaje =  agregarViajeAUnChofer unViaje . elQueMenosViajesTiene . losQueAceptanElViaje unViaje

losQueAceptanElViaje :: Viaje -> [Chofer] -> [Chofer]
losQueAceptanElViaje viaje = filter (puedeTomarViajeUnChofer viaje)


-- 6b. considerar el chofer que menos viaje tenga. Si hay más de un chofer
-- elegir cualquiera.


elQueMenosViajesTiene :: [Chofer] -> Chofer
elQueMenosViajesTiene choferes = foldl unChoferTieneMenosViajesQueOtro (head choferes) choferes

unChoferTieneMenosViajesQueOtro :: Chofer -> Chofer -> Chofer
unChoferTieneMenosViajesQueOtro unChofer otroChofer
        | length (viajesRealizados unChofer) < length (viajesRealizados otroChofer) = unChofer
        | otherwise = otroChofer


-- 6.c efectuar el viaje: esto debe incorporar el viaje a la lista de viajes del
-- chofer. ¿Cómo logra representar este cambio de estado?

agregarViajeAUnChofer :: Viaje -> Chofer -> Chofer
agregarViajeAUnChofer unViaje unChofer = unChofer {viajesRealizados = unViaje : viajesRealizados unChofer }



-- LISTA DE VARIABLES Auxiliares


-- choferDaniel :: Chofer
-- choferDaniel = Chofer "Daniel" 23500 [Viaje (20,04,2017) clienteLucas 150] (aceptaViajeSegunZona "Olivos")

choferJorge :: Chofer
choferJorge = Chofer "Jorge" 23500 [Viaje (20,04,2017) clienteZonaComplicada 200,Viaje (20,04,2018) clienteZonaComplicada 500, Viaje (20,04,2022) clienteZonaComplicada 300 ] aceptaLosViajesMayoresA200


-- choferAlejandra :: Chofer
-- choferAlejandra = Chofer "Alejandra" 180000 [] aceptaCualquierViaje


-- viajeDeEjemplo :: Viaje
-- viajeDeEjemplo = Viaje (20,04,2017) clienteLucas 150
viajeDeCosto400 :: Viaje
viajeDeCosto400 = Viaje (20,04,2017) clienteLucas 400
viajeDeCosto100 :: Viaje
viajeDeCosto100 = Viaje (20,04,2017) clienteLucas 100

viajeDeLeoDeFlores :: Viaje
viajeDeLeoDeFlores = Viaje (20,04,2017) clienteLeo 100

viajeDeLucasDeVictoria :: Viaje
viajeDeLucasDeVictoria = Viaje (20,04,2017) clienteLucas 100

viajeDeJuanDeOlivos :: Viaje
viajeDeJuanDeOlivos = Viaje (20,04,2017) clienteJuan 100

clienteLeo = ("Leo","Flores")

clienteJuan = ("Juan","Olivos")
-- clienteLucas :: Cliente
-- clienteLucas = ("Lucas","Victoria")

-- clienteZonaComplicada :: Cliente
-- clienteZonaComplicada = ("Melli","Olivos")

listaDeChoferes :: [Chofer]
listaDeChoferes = [choferDaniel,choferAlejandra,choferJorge]