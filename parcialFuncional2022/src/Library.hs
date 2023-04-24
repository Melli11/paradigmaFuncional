{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Library where
import PdePreludat

-- 1. Implementar una función que nos recomendaron ya que hará un poco más fácil el resto
-- del desarrollo: hastaQue, que toma una transformación, una condición y un valor y realiza
-- la transformación en el valor las veces que sea necesaria hasta que la condición se cumpla
-- para ese valor.

-- 2 - De una espada conocemos, cuál es su largo en cm, qué tan filosas son (siempre está
-- entre 0 y 10) y qué tan flexibles son (siempre está entre 1 y 5).
-- De eso también se desprende:
-- - a) Cuál es su poder de corte, que es el producto del largo por el filo dividido la
-- flexibilidad.
-- - b) Cuál es su durabilidad, que se calcula como flexibilidad * 30 / metros de largo.

type Largo = Number
type Filo = Number
type Flexibilidad = Number

data Espada = UnaEspada {
        largo :: Largo,
        filo :: Filo,
        flexibilidad :: Flexibilidad
}deriving (Show)


ajustarFilo :: Number -> Espada -> Espada
ajustarFilo filo espada = espada {filo = min (max 0 filo) 10}

ajustarFlexibilidad :: Number -> Espada -> Espada
ajustarFlexibilidad flex espada = espada {flexibilidad = min (max 1 flex) 5}

ajustarLongitud :: Number -> Espada -> Espada
ajustarLongitud centimetros espada = espada {largo = centimetros  }



-- ajustarFilo :: Number -> Number
-- ajustarFilo filo  =  min (max 0 filo) 10

-- ajustarFlexibilidad :: Number -> Number
-- ajustarFlexibilidad flex  =  min (max 1 flex) 5

-- ajustarLongitud :: Number -> Number
-- ajustarLongitud centimetros  = centimetros


-- crearEspada :: Espada -> Espada
-- crearEspada (UnaEspada centimetros filo  flex ) = UnaEspada (ajustarLongitud centimetros) (ajustarFilo filo) (ajustarFlexibilidad flex)

poderDeCorte :: Espada -> Number
poderDeCorte espada  =  (filo espada *  largo espada) / flexibilidad espada

durabilidad :: Espada -> Number
durabilidad  espada = flexibilidad espada * 30 / largo espada * 100


-- a) afilar una espada: mejora su filo en 3 pero se pierden 6cm de largo de la espada.
afilarUnaEspada :: Espada -> Espada
afilarUnaEspada espada = ajustarLongitud (largo espada - 6). ajustarFilo (filo espada + 3) $ espada

-- b) afilar a tope: afilar una espada hasta que tenga el máximo filo.

afilarATope :: Espada -> Espada
afilarATope espada
        | filo espada < 10 =  afilarATope (afilarUnaEspada espada)
        | otherwise = espada

-- c) martillar: se usa un martillo de cierto peso para esto. Alarga una espada en 5cm *
-- cada kilo de peso del martillo, aumenta su flexibilidad en 1 por kilo pero disminuye su
-- filo en 0.5 por kilo.

data Martillo = UnMartillo{ peso :: Number }
-- type Peso = Number
-- type Martillo = Peso

martillar :: Martillo -> Espada -> Espada
martillar unMartillo  = ajustarFilo (0.5 - peso unMartillo) . ajustarFlexibilidad (1 * peso unMartillo) . ajustarLongitud (5 * peso unMartillo)

espadaGenerica :: Espada
espadaGenerica = UnaEspada 0 0 0

crearEspada :: Number -> Number -> Number -> Espada
crearEspada largo filo flex = ajustarLongitud largo . ajustarFilo filo . ajustarFlexibilidad flex $ espadaGenerica

-- Ejemplo de Espada 
espadaEjemplo :: Espada
espadaEjemplo = crearEspada 10 5 5

-- espada base: es un paso intermedio por el que pasan los herreros para hacer las
-- otras espadas, es una espada de 80cm con 0 de filosidad y 1 de flexibilidad.

espadaBase :: Espada
espadaBase = crearEspada 80 0 1

-- i) katana: es una espada base martillada una vez con un martillo de 3 kilos y luego
-- afilada a tope.
-- Su poder de corte debería ser 177.5 y su durabilidad debería estar entre 169 y 170.

martilloDe3Kilos :: Martillo
martilloDe3Kilos = UnMartillo 3
martilloDe4Kilos = UnMartillo 4
martilloDe20Kilos = UnMartillo 20

-- ii) katana 
katana :: Espada
katana = afilarATope $ martillar martilloDe3Kilos espadaBase

-- iii) espada larga: es una espada base que fue afilada 2 veces y luego martillada con
-- un martillo de 4 kilos hasta alcanzar al menos 125cm.
-- Su poder de corte debería ser 408 y su durabilidad debería estar entre 44 y 45

-- espadaLarga martilloDe4Kilos . afilarUnaEspada . afilarUnaEspada
-- hastaQue = (largo)

-- v) Masamune: es una katana martillada con un martillo de 20kg y luego afilada a
-- tope.
-- Su poder de corte debería ser 294 y su durabilidad estar entre 102 y 103

masamune :: Espada
masamune = afilarATope $ martillar martilloDe20Kilos espadaBase

espadasLegendarias :: [Espada] -> [Espada]
espadasLegendarias = filter esLegendaria

esLegendaria :: Espada -> Bool
esLegendaria unaEspada = poderDeCorte unaEspada > 250 && durabilidad unaEspada > 100

-- a) golpear una roca, lo cual disminuye la longitud de la espada en 10cm - la
-- durabilidad (con mínimo 0) y disminuye la flexibilidad en 2.
-- Ejemplo: golpear una roca con una katana nos deja una espada con 71cm y
-- flexibilidad de 2

golpearUnaRoca unaEspada = ajustarLongitud (10 - durabilidad unaEspada) . ajustarFlexibilidad 2 $unaEspada 

-- b) chocar con otra espada, lo cual disminuye la flexibilidad y el filo de la espada en el
-- desgaste de combate. El desgaste de combate al chocar una espada contra otra se
-- calcula como el cociente entre el poder de corte de la espada contraria y la
-- durabilidad de nuestra espada

chocarContraOtraEspada :: Espada -> Espada -> Espada
chocarContraOtraEspada unaEspada otraEspada = unaEspada {filo = filo unaEspada - desgasteDelCombate unaEspada otraEspada , flexibilidad = flexibilidad unaEspada - desgasteDelCombate unaEspada otraEspada }

desgasteDelCombate :: Espada -> Espada -> Number
desgasteDelCombate unaEspada otraEspada = poderDeCorte otraEspada / durabilidad unaEspada 

-- 6)
-- prueba básica: golpear una roca 3 veces y luego afilar la espada una vez. Si
-- el poder de corte es al menos 650 al final entonces pasó la prueba.
-- Deberían pasarla todas las espadas menos la base

pruebaBasica :: Espada -> Bool 
pruebaBasica =  (==650). poderDeCorte . afilarUnaEspada . golpearUnaRoca. golpearUnaRoca . golpearUnaRoca  


-- Queremos saber si una espada pasa una prueba de control de calidad.

pasoControlDeCalidad unaEspada unaPrueba = unaPrueba unaEspada

pruebaDeCalidad 