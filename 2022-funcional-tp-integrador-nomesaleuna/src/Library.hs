{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Evaluate" #-}
module Library where
import PdePreludat
import GHC.Arr (unsafeAccumArray)
import Control.Exception (asyncExceptionFromException)

-- De cada planeta sabemos su nombre, su posición en el espacio (que tiene 3 coordenadas: x, y y z) y una fórmula que indica a cuánto tiempo terrestre equivale pasar un año allí.

type Coordenada = (Number,Number,Number)
type ConstantePlanetaria = Number
type Años = Number

data Planeta = UnPlaneta {
    nombrePlaneta :: String,
    posicion :: Coordenada,
    conversionTemporal :: Años -> Años
}deriving(Show , Eq )

-- De los astronautas sabemos el nombre, su edad terrestre y el planeta en el que se encuentran.
data Astronauta = UnAstronauta {
    nombre:: String,
    edadTerrestre :: Años,
    planetaActual :: Planeta
} deriving(Show)

-- 1a) Saber la distancia entre 2 planetas sabiendo que la distancia entre dos posiciones se calcula de esta forma:


distancia :: Coordenada -> Coordenada -> Number
-- Alternativa 1 : Directo 
-- distancia coordenada1 coordenada2 = sqrt (sumarCoordenadas (restarYelevarAlCuadrado coordenada1 coordenada2))

-- Alternativa 2 : Aplicando composición y aplicación parcial 
distancia coordenada1 = sqrt . sumarCoordenadas . restarYelevarAlCuadrado coordenada1

-- Alternativa 1: Usando funcion auxiliar aplicada parcialmente (Mejora expresividad)
restarYelevarAlCuadrado :: Coordenada -> Coordenada -> Coordenada
restarYelevarAlCuadrado  = funcionVectorial restarYCuadrado
-- restarYelevarAlCuadrado coordenada1 coordenada2 = funcionVectorial restarYCuadrado coordenada1 coordenada2 

-- Alternativa 2: Usando Lambda
-- restarYelevarAlCuadrado' :: Coordenada -> Coordenada -> Coordenada
-- restarYelevarAlCuadrado' coordenada1 coordenada2 = funcionVectorial (\n1 n2 -> (n1 - n2) ^2) coordenada1 coordenada2 

-- funcionVectorial :: (a,a,a) -> (b,b,b) -> (a -> b -> c) -> (c,c,c)
funcionVectorial :: (Number -> Number -> Number) -> Coordenada -> Coordenada  -> Coordenada
funcionVectorial funcion  (x_Punto1, y_Punto1, z_Punto1) (x_Punto2, y_Punto2, z_Punto2) = (funcion x_Punto1 x_Punto2, funcion y_Punto1 y_Punto2, funcion z_Punto1 z_Punto2)

restarYCuadrado :: Number -> Number -> Number
restarYCuadrado n1 n2 = (n1 - n2) ^ 2

sumarCoordenadas :: Coordenada -> Number
sumarCoordenadas (x, y, z) = x + y + z

distanciaEntrePlanetas :: Planeta -> Planeta -> Number
distanciaEntrePlanetas planeta1 planeta2 = distancia (posicion planeta1) (posicion planeta2)


-- 1b) Saber cuánto tiempo se tarda en viajar de un planeta a otro yendo a una determinada velocidad, que es la distancia entre ambos dividido por la velocidad de viaje.

tiempoRequerido :: Number -> Planeta -> Planeta -> Number
tiempoRequerido velocidad planetaA planetaB =  distancia  (posicion planetaA) (posicion planetaB) / velocidad

-- 2) Poder hacer que un astronauta pase una determinada cantidad de años en su planeta actual, lo cual debería aumentar su edad terrestre en la cantidad de tiempo que el planeta indique a partir de los años indicados.

transitarAnios :: Astronauta -> Años -> Astronauta
transitarAnios unAstronauta cantidadDeAnios = unAstronauta {edadTerrestre = conversionTemporal (planetaActual unAstronauta) cantidadDeAnios + edadTerrestre unAstronauta }

-- 3) Queremos que un astronauta pueda viajar a otro planeta usando una nave determinada. 
-- Una nave es una función que dados dos planetas (origen y destino) retorna el tiempo requerido para viajar entre ellos.

-- a) La nave vieja cuya velocidad es 7 m/s a menos que tenga menos de 6 tanques de oxígeno, en cuyo caso viaja a 10 m/s.

type Nave = Planeta -> Planeta -> Number
type TanquesDeOxigeno = Number

data DiseñoNave = NaveVieja {tanques :: TanquesDeOxigeno} |
                  NaveFuturista

criterioNaveVieja :: Number -> Number
criterioNaveVieja cantidadDeTanquesOxigeno
        | cantidadDeTanquesOxigeno >= 6 = 10
        | otherwise = 7

crearNave :: DiseñoNave -> Nave
crearNave NaveFuturista = (\planeta1 planeta2 -> 0)
crearNave (NaveVieja cantidadTanques) = tiempoRequerido (criterioNaveVieja cantidadTanques)

-- naveConstante :: Number -> Nave
-- naveConstante tiempo _ _ = tiempo
-- naveFuturista :: Nave
-- naveFuturista = naveConstante 0
-- naveVieja :: Number -> Nave
-- naveVieja cantidadDeTanquesOxigeno
--         | cantidadDeTanquesOxigeno >= 6 = tiempoRequerido 10
--         | otherwise = tiempoRequerido 7

-- naveViejaCincoTanques :: Nave
-- naveViejaCincoTanques = naveVieja 5
-- naveViejaOchoTanques :: Nave
-- naveViejaOchoTanques = naveVieja 8


diseñoDeNaveViejaCincoTanques = NaveVieja 5
diseñoDeNaveViejaOchoTanques = NaveVieja 8

diseñoDeNaveFuturista :: DiseñoNave
diseñoDeNaveFuturista = NaveFuturista

-- Ejemplo de nave

unaNaveViejaDeCincoTanques :: Nave
unaNaveViejaDeCincoTanques = crearNave diseñoDeNaveViejaCincoTanques

unaNaveViejaDeOchoTanques = crearNave diseñoDeNaveViejaOchoTanques

unaNaveFuturista :: Nave
unaNaveFuturista = crearNave NaveFuturista

-- naveVieja :: Number -> Nave
-- Alternativa 1
-- naveVieja tanquesOxigeno = nave (criterioNaveVieja tanquesOxigeno)
-- Alternativa 2  
-- naveVieja  = crearNave . criterioNaveVieja

-- b) La nave futurista que viaja tan rápido que el tiempo de viaje es despreciable.

-- naveFuturista :: Nave
-- naveFuturista = 0

-- c) Realizar un viaje implica que el astronauta aumente su edad en el tiempo de viaje correspondiente para llegar al destino elegido y cambie de planeta al mismo.

realizarViaje :: Astronauta -> Planeta -> Nave -> Astronauta
-- realizarViaje  unastronauta planetaInicial planetaDestino nave = unAstronauta {edadTerrestre = (Nave planetaInicial planetaDestino) + edadTerrestre, planetaActual = planetaDestino}
realizarViaje  unAstronauta  planetaDestino nave = unAstronauta {edadTerrestre = (nave (planetaActual unAstronauta) planetaDestino) + (edadTerrestre unAstronauta), planetaActual = planetaDestino}

-- usar transitar anios
-- transitarAnios :: Astronauta -> Años -> Astronauta
-- transitarAnios unAstronauta cantidadDeAnios = unAstronauta {edadTerrestre = conversionTemporal (planetaActual unAstronauta) cantidadDeAnios + edadTerrestre unAstronauta }


-- 4a) Hacer que un grupo de astronautas rescate a un astronauta que quedó varado en otro planeta usando una determinada nave. 
-- Lo que se espera como resultado de efectuar un rescate es la lista de astronautas luego de que todos los rescatistas viajen
--  en la nave a buscar al astronauta al planeta donde está varado, incorporen a la tripulación al rescatado tras pasar el tiempo correspondiente
--  en ese planeta y luego viajen todos en la misma nave al planeta de donde vinieron los rescatistas.
-- Se puede asumir que todos venían del mismo planeta origen, y el tiempo que tiene que esperar el astronauta a rescatar es el que tarda la nave en ir de un lado a otro.

data GrupoDeRescatistas = GrupoRescatistas{
    rescatistas :: [Astronauta],
    nave :: Nave
}deriving(Show )

-- 1ra IMPLEMENTACIÓN
viajeRescate'' ::  [Astronauta] -> Nave -> Astronauta -> [Astronauta]
viajeRescate'' grupoRescatista nave astronautaVarado  = map (\astronauta -> realizarViaje astronauta (planetaActual (head grupoRescatista)) nave) (astronautaVarado : map (\astronauta -> realizarViaje astronauta (planetaActual astronautaVarado) nave) grupoRescatista)

-- 2dA IMPLEMENTACIÓN USANDO 
-- Todo lo que es Nave -> [Astronauta] lo cambie por GrupoRescatista
--el varado tiene que pasar X tiempo en el planeta hasta que llegue el equipo. Transitar Anios.
-- lograr abstaccion entre ida y vuelta

-- viajeRescate' :: Astronauta -> GrupoDeRescatistas -> [Astronauta]
realizarViajeDeRescate :: Astronauta -> GrupoDeRescatistas -> [Astronauta]
realizarViajeDeRescate astronautaVarado grupoDeRescate = (viajeDeRegreso grupoDeRescate) $agregarAlEquipoDeRescate astronautaVarado $viajeDeIda astronautaVarado grupoDeRescate       



-- viaje de ida: actualiza el destino y la edad de los astronautas
viajeDeIda :: Astronauta -> GrupoDeRescatistas -> GrupoDeRescatistas  
viajeDeIda astronautaVarado grupoDeRescate = grupoDeRescate {rescatistas = map (\astronauta -> realizarViaje astronauta (planetaActual astronautaVarado) (nave grupoDeRescate) ) (rescatistas grupoDeRescate)}

-- viajeDeIda' ryan equipoDeRescate

esperar :: Astronauta -> GrupoDeRescatistas -> Astronauta
esperar = implementame

agregarAlEquipoDeRescate :: Astronauta -> GrupoDeRescatistas -> GrupoDeRescatistas
agregarAlEquipoDeRescate unAstronauta grupoDeRescate = grupoDeRescate {rescatistas = (unAstronauta : rescatistas grupoDeRescate)}

viajeDeRegreso :: GrupoDeRescatistas -> GrupoDeRescatistas -> [Astronauta]
viajeDeRegreso grupoDestino grupoDeRescate   = map (\astronauta -> realizarViaje astronauta (planetaActual (head (rescatistas grupoDestino))) (nave grupoDestino)) $rescatistas grupoDeRescate 


-- 4b) Dado un grupo de astronautas rescatistas , la nave que usan y un grupo de astronautas que quedaron varados en otros planetas, 
-- los nombres de los astronautas varados que podrían ser rescatados. Un astronauta puede ser rescatado por 
-- los rescatistas si luego del rescate ninguno de los astronautas (incluyendo al rescatado) tiene más de 90 años.

nombresDeAstronautasRescatables :: GrupoDeRescatistas -> [Astronauta] -> [String]
nombresDeAstronautasRescatables grupoDeRescate grupoDeAstronautasVarados = map (nombre) $ filter (puedeSerRescatado grupoDeRescate) grupoDeAstronautasVarados 

puedeSerRescatado :: GrupoDeRescatistas -> Astronauta -> Bool
puedeSerRescatado grupoDeRescate unAstronautaVarado = all ((<90).edadTerrestre) $ realizarViajeDeRescate unAstronautaVarado grupoDeRescate


-- VARIABLES AUXILIARES 
equipoDeRescateConNaveViejaCincoTanques :: GrupoDeRescatistas
equipoDeRescateConNaveViejaCincoTanques = GrupoRescatistas [astroboy,megaman,martian] unaNaveViejaDeCincoTanques

equipoDeRescateConNaveFuturista :: GrupoDeRescatistas
equipoDeRescateConNaveFuturista = GrupoRescatistas [astroboy,megaman,martian] unaNaveFuturista

equipoDeRescateConNaveViejaDeOchoTanques :: GrupoDeRescatistas
equipoDeRescateConNaveViejaDeOchoTanques = GrupoRescatistas [astroboy,megaman,martian] unaNaveViejaDeOchoTanques

equipoDeRescateConNaveViejaDeOchoTanquesFinal :: GrupoDeRescatistas
equipoDeRescateConNaveViejaDeOchoTanquesFinal = GrupoRescatistas [ryan,astroboy,megaman,martian] unaNaveViejaDeOchoTanques

listaDeAstronautasOld :: [Astronauta]
listaDeAstronautasOld = [astroOld,megamanOld,martianOld]

listaDeAstronautasBaby :: [Astronauta]
listaDeAstronautasBaby = [astroboy,megaman,martian]

equipoDeAstronautasVarados :: [Astronauta]
equipoDeAstronautasVarados = [pinki,cerebro,tommy,jazz] 

-- Ejemplo de Astronautas

viajero :: Astronauta
viajero = UnAstronauta "Viajero" 50 planetaX

astroboy :: Astronauta
astroboy = UnAstronauta "Astro" 20 namek

astroOld :: Astronauta
astroOld = UnAstronauta "AstroOld" 100 namek

megaman :: Astronauta
megaman = UnAstronauta "Megaman" 22 namek

megamanOld :: Astronauta
megamanOld = UnAstronauta "MegamanOld" 122 namek

martian :: Astronauta
martian = UnAstronauta "Martian" 23 namek

martianOld :: Astronauta
martianOld = UnAstronauta "MartianOld" 123 namek
terricola :: Astronauta
terricola = UnAstronauta "Terricola" 30 planetaTierra

ryan :: Astronauta
ryan = UnAstronauta "Ryan" 25 planetaX

yoda :: Astronauta
yoda = UnAstronauta "Yoda" 5 pluton

darthVader :: Astronauta
darthVader = UnAstronauta "Vader" 5 planetaTierra

pinki :: Astronauta
pinki = UnAstronauta "Pinki" 10 planetaMuerte

cerebro :: Astronauta
cerebro = UnAstronauta "Cerebro" 10 planetaX
tommy :: Astronauta
tommy = UnAstronauta "Tommy" 50 namek
jazz :: Astronauta
jazz = UnAstronauta "Jazz" 50 namek

-- Coordenadas de ejemplo

puntoA :: Coordenada
puntoA = (1,1,1)
puntoB :: Coordenada
puntoB = (2,2,2)

--- Planetas de ejemplo
namek :: Planeta
namek = UnPlaneta "namek" (1,1,1) (*5)
solaris :: Planeta
solaris = UnPlaneta "solaris" (100,100,100) (\anios -> anios*3 )
planetaX :: Planeta
planetaX = UnPlaneta "planetaX" (50,50,50) (\anios -> anios*0.5 )
planetaTierra :: Planeta
planetaTierra = UnPlaneta "tierra" (250,250,250) (\anios -> anios*1 )
pluton :: Planeta
pluton = UnPlaneta "pluton" (500,500,500) (\anios -> anios*10 )
planetaMuerte :: Planeta
planetaMuerte = UnPlaneta "planetaMuerte" (1000,1000,1000) (\anios -> anios*10 )






doble :: Number -> Number
doble algo = algo * 2