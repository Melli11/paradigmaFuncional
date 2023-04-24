module Library where
import PdePreludat

data Auto = Auto {
    color :: Color,
    velocidad :: Number,
    distancia :: Number
}deriving (Show,Eq)

type Carrera = ( Auto , Posicion)
type Posicion = Number

data Color = Rojo | Verde | Amarillo | Azul deriving(Show,Eq)
  -- auto = Auto Color Vel Dist

autoAmarillo = Auto Amarillo 50 300
autoRojo = Auto Rojo 100 250
autoVerde = Auto Verde 75 200
autoAzul = Auto Azul 50 150

listaDeAutosWinRojo = [autoRojo,autoVerde,autoAzul,autoAmarillo]

laDistanciaEsMenorA10 :: Auto -> Auto -> Bool
laDistanciaEsMenorA10 unAuto deOtroAuto = abs(distancia unAuto - distancia deOtroAuto) <10

losAutosSonDiferentes :: Auto -> Auto -> Bool
losAutosSonDiferentes unAuto deOtroAuto = color unAuto /= color deOtroAuto

estaCerca :: Auto -> Auto -> Bool
estaCerca unAuto deOtroAuto = laDistanciaEsMenorA10 unAuto deOtroAuto && losAutosSonDiferentes unAuto deOtroAuto

-- -- vaTranquilo :: Auto -> Auto -> Bool 
-- vaTranquilo :: Auto -> [Auto] -> Bool
nadieCerca :: Auto -> [Auto] -> Bool
nadieCerca unAuto =    any ((False ==) . estaCerca unAuto) . filter (/= unAuto)

vaTranquilo :: Auto -> [Auto] -> Bool
vaTranquilo unAuto competidores = nadieCerca unAuto competidores && lesVaGanandoATodos unAuto competidores

lesVaGanandoATodos :: Auto -> [Auto] -> Bool
lesVaGanandoATodos unAuto = all (mayorDistanciaDeUnAuto unAuto) . filter (/= unAuto)

-- funcion auxiliar
lesVaGanandoA :: Auto -> [Auto] -> [Auto]
lesVaGanandoA unAuto = filter ((==True).mayorDistanciaDeUnAuto unAuto) . filter (/= unAuto)
-- con filter (/= unAuto) me quedo con todos los autos diferentes del que estoy preguntando

mayorDistanciaDeUnAuto ::Auto -> Auto -> Bool
mayorDistanciaDeUnAuto unAuto otroAuto = distancia unAuto > distancia otroAuto

-- autosQueLoSuperan :: Auto ->
autosQueLoSuperan :: Auto -> [Auto] -> Number
autosQueLoSuperan unAuto = length.filter (==False) . map (mayorDistanciaDeUnAuto unAuto) . filter (/= unAuto)

enQuePuestoEsta :: Auto -> [Auto] -> Number
enQuePuestoEsta unAuto listaDeAutos= 1 + autosQueLoSuperan unAuto listaDeAutos

-- 2. Desarrollar las funciones necesarias para manipular el estado de los autos para que sea posible:

-- Hacer que un auto corra durante un determinado tiempo. Luego de correr la cantidad de tiempo
-- indicada, la distancia recorrida por el auto debería ser equivalente a la distancia que llevaba
-- recorrida + ese tiempo * la velocidad a la que estaba yendo

correrDuranteUnTiempo :: Auto -> Number -> Auto
correrDuranteUnTiempo auto tiempo = auto {distancia = distancia auto + tiempo * velocidad auto}


alterarLaVelocidad' :: (Number -> Number ) -> Auto -> Auto
alterarLaVelocidad'  modificador auto = auto {velocidad = modificador (velocidad auto)}

alterarLaVelocidad:: (Number -> Number ) -> Auto  -> Auto
alterarLaVelocidad    modificador auto = auto {velocidad = modificador.velocidad $auto}

bajarVelocidad :: Number -> Auto -> Auto
bajarVelocidad cantidad = alterarLaVelocidad ( max 0 . subtract cantidad )

-- :t subtract 
-- subtract :: Number -> Number -> Number
-- Ejemplo de uso: Substract resta el primer numerico que recibe al segundo

-- Input: subtract 3 5
-- Output: 2

-- 3. Power UPS

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

-- Inicialmente queremos poder representar los siguientes power ups, pero debería ser fácil incorporar más
-- power ups a futuro para enriquecer nuestro programa:

-- a. terremoto: luego de usar este poder, los autos que están cerca del que gatilló el power up bajan
-- su velocidad en 50

terremoto' :: Auto -> (Auto -> Auto) -> [Auto] -> [Auto]
terremoto' unAuto competidores = map(bajarVelocidad 50).afectarALosQueCumplen (estaCerca unAuto ) competidores

terremoto :: Auto -> Number-> (Auto -> Auto) -> [Auto] -> [Auto]
terremoto unAuto cantidad = miguelitos unAuto 50

-- este poder debe permitir configurarse con una cantidad que indica en cuánto deberán
-- bajar la velocidad los autos que se vean afectados por su uso. Los autos a afectar son aquellos a
-- los cuales el auto que gatilló el power up les vaya ganando

miguelitos :: Auto -> Number -> (Auto -> Auto) -> [Auto] -> [Auto]
miguelitos unAuto cantidad competidores  =  map (bajarVelocidad cantidad) . afectarALosQueCumplen (mayorDistanciaDeUnAuto unAuto) competidores

jetpack :: Auto -> Number -> Auto
jetpack unAuto duracion = foldl aplicarPowers unAuto [alterarLaVelocidad (*2),flip correrDuranteUnTiempo  duracion ,bajarVelocidad (velocidad unAuto)]
aplicarPowers  unAuto f = f unAuto


--  jet pack: este poder debe afectar, dentro de la carrera, solamente al auto que gatilló el poder. El
-- jet pack tiene un impacto que dura una cantidad limitada de tiempo, el cual se espera poder
-- configurar.
-- Cuando se activa el poder del jet pack, el auto afectado duplica su velocidad actual, luego corre
-- durante el tiempo indicado y finalmente su velocidad vuelve al valor que tenía antes de que se
-- active el poder.
-- Por simplicidad, no se espera que los demás autos que participan de la carrera también avancen
-- en ese tiempo.

-- 4. Desarrollar la función:
-- simularCarrera :: Carrera -> [Carrera -> Carrera] -> [(Int, Color)]

