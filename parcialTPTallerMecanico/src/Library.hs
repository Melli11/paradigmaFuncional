{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use section" #-}
module Library where
import PdePreludat
import System.IO (hReady)

type Desgaste = Number
type Patente = String
type Fecha = (Number, Number, Number)

-- Definiciones base 
anio :: Fecha -> Number
anio (_, _, year) = year

data Auto = Auto {
 patente :: Patente,
 desgasteLlantas :: [Desgaste],
 rpm :: Number,
 temperaturaAgua :: Number,
 ultimoArreglo :: Fecha
} deriving Show

-- -- Punto 1
-- Saber el costo de reparación de un auto 
-- ● si la patente tiene 7 dígitos, es $ 12.500 
-- ● si no, si la patente está entre las letras "DJ" y "NB", se aplica el calculoPatental 
-- ○ que es $ 3.000 multiplicado por la longitud para las patentes que terminen en  4 
-- ○ o $ 20.000 para el resto de las patentes 
-- ● de lo contrario, se le cobra $ 15000 

type Costo = Number

costoDeReparacion:: Auto -> Costo
costoDeReparacion auto
    |   longitudDeLaPatente auto == 7 = 12500
    |   condicionDeLaPatente auto = calculoPatental auto
    |   otherwise = 15000

longitudDeLaPatente :: Auto -> Number
longitudDeLaPatente auto = length $ patente auto

condicionDeLaPatente' :: Auto -> Bool
condicionDeLaPatente' (Auto ['D','J',_,_,_,_] _ _ _ _) = True --Formato viejo posee 6 caracteres 
condicionDeLaPatente' (Auto [_,_,_,_,'N','B'] _ _ _ _) = True --Formato viejo posee 6 caracteres
condicionDeLaPatente' _ = False

condicionDeLaPatente :: Auto -> Bool
condicionDeLaPatente auto =  patente auto >= "DJ" && patente auto <= "NB" 

calculoPatental :: Auto -> Number
calculoPatental auto
    |    ('4' ==) . last  $  patente auto  = 3000 * longitudDeLaPatente auto
    |   otherwise = 20000

-- Punto 2 
-- ATENCIÓN: Resolver únicamente con Composición y aplicación parcial
-- Parte 1) Auto peligroso  
-- Dado un auto, saber si es peligroso. Esta condición se cumple cuando el desgaste de la 
-- primera llanta es mayor a 0.5


elAutoEsPeligroso :: Auto -> Bool
elAutoEsPeligroso = (>0.5).head.desgasteLlantas

-- Parte 2) Necesita revisión  
-- Dado un auto, saber si necesita revisión. Esta condición se cumple cuando el último arreglo 
-- fue realizado en el año 2015 ó antes. 

necesitaRevisión :: Auto -> Bool
necesitaRevisión = (<=2015).anio.ultimoArreglo


-- Punto 3: Personal técnico encargado de las reparaciones

-- Necesitamos definir a las siguientes personas que realizan actividades en el taller mecánico: 

type Tecnico = Auto -> Auto

-- ● Alfa: hace que el auto regule a 2.000 vueltas, salvo que esté a menos de 2.000 
-- vueltas, en cuyo caso lo deja como está 
alfa :: Tecnico
alfa auto = auto {rpm = chequearYajustarRPM auto}

chequearYajustarRPM :: Auto -> Number
chequearYajustarRPM auto
    | rpm auto < 2000 =  rpm auto
    | otherwise = 2000

-- ● Bravo: cambia todas las cubiertas, dejándolas sin desgaste 

bravo :: Tecnico
bravo auto = auto {desgasteLlantas = []}

-- ● Charly:  realiza las mismas actividades que Alfa y Bravo

charly :: Tecnico
charly = alfa.bravo

-- Necesitamos definir a las siguientes personas que realizan actividades en el taller mecánico

-- ● Tango: le gusta decir que hizo muchas cosas, pero en realidad no hace ningún arreglo 

tango :: Tecnico
tango = id

-- ● Zulu: revisa la temperatura del agua, la deja a 90 y hace lo mismo que Lima (ver a 
-- continuación) 

zulu :: Tecnico
zulu = lima . ajustarTemperatura 90

-- ajustarTemperatura  :: Auto -> Auto
ajustarTemperatura :: Number -> Auto -> Auto
ajustarTemperatura temperatura auto  = auto { temperaturaAgua = temperatura }

-- ● Lima:  cambia las cubiertas delanteras (las dos primeras), dejándolas sin desgaste. Las 
-- posteriores quedan igual

-- type Desgaste = Number
--  desgasteLlantas :: [Desgaste],


lima :: Tecnico
-- lima (Auto patente (x:y:xs) rpm temp ultArreglo ) = Auto patente [0,0]:xs rpm temp ultArreglo 
lima = cambiarCubiertasDelanteras

cambiarCubiertasDelanteras :: Auto -> Auto
cambiarCubiertasDelanteras auto = auto { desgasteLlantas = [0,0] ++ drop 2 (desgasteLlantas auto)}


-- Punto 4: Ordenamiento TOC de autos
-- Dada una serie de autos, saber si están ordenados en base al siguiente criterio: 
-- ● los autos ubicados en la posición impar de la lista deben tener una cantidad de 
-- desgaste impar 

-- ● los autos ubicados en la posición par deben tener una cantidad de desgaste par 
-- ● asumimos que el primer elemento está en la posición 1, el segundo elemento en la 
-- posición 2, etc. 


-- Punto 5: Orden de reparación (Común para ambos integrantes)  
-- Aplicar una orden de reparación, que tiene 
-- ● una fecha 
-- ● una lista de técnicos 
-- y consiste en que cada uno de los técnicos realice las reparaciones que sabe sobre el auto, al 
-- que además hay que actualizarle la última fecha de reparación.

data OrdenDeReparacion = OrdenDeReparacion {
    fecha :: Fecha,
    listaDeMecanicos :: [Tecnico]
}

-- Auto de ejemplo 

autoX :: Auto
autoX = Auto "ABCED" [1,5,6,10] 10 10 (10,12,1990)

aplicarUnaOrdenDeReparacion :: Auto -> OrdenDeReparacion -> Auto
aplicarUnaOrdenDeReparacion auto orden  = foldl aplicarUnaReparacion auto (listaDeMecanicos orden)

aplicarUnaReparacion :: Auto -> Tecnico -> Auto
aplicarUnaReparacion  auto mecanico = mecanico auto

-- Punto 6
-- Parte 1) Integrante a: Técnicos que dejan el auto en 
-- condiciones

-- Dada una lista de técnicos, determinar todos aquellos técnicos que dejarían 
--  el auto en condiciones. (Que no sea peligroso andar, recordar el 
-- punto 2.1 del integrante a).

dejanElAutoEnCondiciones ::  [Tecnico] -> Auto -> [Tecnico]
dejanElAutoEnCondiciones  tecnicos auto =  filter (flip elTecnicoLoDejaEnCondiciones auto) tecnicos

elTecnicoLoDejaEnCondiciones :: Tecnico -> Auto -> Bool
elTecnicoLoDejaEnCondiciones tecnico auto =  noEsPeligroso $ tecnico  auto

-- elAutoEsPeligroso :: Auto -> Bool
-- elAutoEsPeligroso = (>0.5).head.desgasteLlantas

noEsPeligroso :: Auto -> Bool
noEsPeligroso = not.elAutoEsPeligroso

-- Parte 2) Integrante b: Costo de reparación de autos que necesitan revisión 
-- Dada una lista de autos, saber cuál es el costo de reparación de los 
-- autos que necesitan revisión.


costoDelArreglo :: [Auto] -> Number
costoDelArreglo = sum.map costoDeReparacion . losQueNecesitanRevision

losQueNecesitanRevision :: [Auto] -> [Auto]
losQueNecesitanRevision = filter necesitaRevisión