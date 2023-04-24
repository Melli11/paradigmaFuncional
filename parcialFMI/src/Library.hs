{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Library where
import PdePreludat
import GHC.Generics (Par1(unPar1))

doble :: Number -> Number
doble numero = numero + numero
data RecursoNatural = Mineria | Petroleo | IndustriaPesada | Ganaderia | Ecoturismo deriving (Show,Eq)

data Pais = Pais {
    ingresoPerCapita :: Number,
    sectorPublico :: Number,
    sectorPrivado :: Number,
    recursos :: [RecursoNatural],
    deuda :: Number
} deriving (Show,Eq)

-- Implementar las estrategias que forman parte de las recetas del FMI.
-- Estrategias/RECETAS

-- prestarle n millones de dólares al país, esto provoca que el país se endeude en
-- un 150% de lo que el FMI le presta (por los intereses)

prestarleNMillones:: Number -> Estrategia
prestarleNMillones cantidad  =  cambiarLaDeuda (cantidad*1.5)

-- reducir x cantidad de puestos de trabajo del sector público, lo que provoca que
-- se reduzca la cantidad de activos en el sector público y además que el ingreso
-- per cápita disminuya en 20% si los puestos de trabajo son más de 100 ó 15% en
-- -- -- caso contrario
reducirSectorPublicoEn :: Number -> Estrategia
reducirSectorPublicoEn cantidad unPais
    | sectorPublico unPais > 100 = cambiarSectorPublico (negate cantidad) $ cambiarIngresoPerCapital (negate (0.2 * ingresoPerCapita unPais)) unPais
    | otherwise =  cambiarSectorPublico (negate cantidad) $ cambiarIngresoPerCapital(negate (0.15 * ingresoPerCapita unPais)) unPais

darEmpresaAlFmi :: RecursoNatural -> Estrategia
darEmpresaAlFmi recursoNatural  = explotarRecurso recursoNatural .  cambiarLaDeuda (negate 2000000)

-- establecer un “blindaje”, lo que provoca prestarle a dicho país la mitad de su
-- Producto Bruto Interno (que se calcula como el ingreso per cápita multiplicado
-- por su población activa, sumando puestos públicos y privados de trabajo) y
-- reducir 500 puestos de trabajo del sector público. Evitar la repetición de código.

establecerBlindaje :: Estrategia
establecerBlindaje unPais = prestarleNMillones (0.5*productoBrutoInterno unPais) $ cambiarSectorPublico (negate 500) unPais

productoBrutoInterno :: Pais -> Number
productoBrutoInterno unPais = ingresoPerCapita unPais * poblacionActiva unPais
-- productoBrutoInterno' unPais= (ingresoPerCapita unPais * )  .  poblacionActiva 

poblacionActiva :: Pais -> Number
poblacionActiva unPais = sectorPrivado unPais + sectorPrivado unPais

-- Dar un ejemplo de cómo generar al país Namibia, cuyo ingreso per cápita
-- es de 4140 u$s, la población activa del sector público es de 400.000, la
-- población activa del sector privado es de 650.000, su riqueza es la minería
-- y el ecoturismo y le debe 50 (millones de u$s) al FMI.

namibia :: Pais
namibia = Pais 4140 400 650000 [Mineria,Ecoturismo] 50000000

bolivia :: Pais
bolivia = Pais 4140 400 650000 [Petroleo,Ecoturismo] 40000000

venezuela :: Pais
venezuela = Pais 4140 400 650000 [Petroleo,Ecoturismo] 30000000

brasil :: Pais
brasil = Pais 4140 400 650000 [Ganaderia,Ecoturismo] 20000000


-- Funciones Auxiliares

type Estrategia = Pais -> Pais
explotarRecurso :: RecursoNatural -> Estrategia
explotarRecurso recursoNatural unPais = unPais { recursos = filter (recursoNatural/=) $recursos unPais  }
cambiarLaDeuda :: Number -> Estrategia
cambiarLaDeuda  cantidad unPais =  unPais { deuda = deuda unPais + cantidad }

cambiarSectorPublico :: Number -> Estrategia
cambiarSectorPublico cantidad unPais = unPais {sectorPublico = sectorPublico unPais + cantidad}

cambiarIngresoPerCapital :: Number -> Estrategia
cambiarIngresoPerCapital cantidad unPais = unPais {ingresoPerCapita = ingresoPerCapita unPais + cantidad}



-- Paises Auxiliares
argentina :: Pais
argentina = Pais 5 10 10 [Mineria, Petroleo] 10000000

listaDePaises :: [Pais]
listaDePaises = [brasil,venezuela,bolivia,argentina,namibia]

-- 3a Modelar una receta que consista en prestar 200 millones, y darle a una
-- empresa X la explotación de la “Minería” de un país.

type Receta = [Estrategia]

recetaDeLaMuerte :: [Estrategia]
recetaDeLaMuerte = [prestarleNMillones 200 ,darEmpresaAlFmi Mineria]

aplicarUnaReceta :: Pais -> Receta -> Pais
aplicarUnaReceta pais receta = foldl aplicarUnaEstrategiaDelFmi pais receta

-- aplicarReceta receta pais = foldr ($) pais receta
-- aplicarReceta receta pais = foldl (\pais estrategia -> estrategia pais) pais receta
aplicarUnaEstrategiaDelFmi unPais estrategia = estrategia unPais

-- 3b Ahora queremos aplicar la receta del punto 3.a al país Namibia (creado en
-- el punto 1.b). Justificar cómo se logra el efecto colateral

-- tiene efecto colateral, efecto de lado o efecto secundario si esta, además de retornar un valor, modifica el estado de su entorno.

-- Si consulto por consola: 
-- Spec Library Spec> namibia
-- Pais {ingresoPerCapita = 4140, sectorPublico = 400, sectorPrivado = 650000, recursos = [Mineria,Ecoturismo], deuda = 50000000}

-- Y luego aplico la receta de la muerta a namibia obtengo:
-- Spec Library Spec> aplicarUnaReceta namibia recetaDeLaMuerte
-- Pais {ingresoPerCapita = 4140, sectorPublico = 400, sectorPrivado = 650000, recursos = [Ecoturismo], deuda = 48000300}



-- 1b). Justificar cómo se logra el efecto colateral

-- a.Dada una lista de países conocer cuáles son los que pueden zafar,
-- aquellos que tienen "Petróleo" entre sus riquezas naturales.

puedenZafar :: [Pais] -> [Pais]
puedenZafar paises = filter (elem Petroleo . recursos ) paises


-- b. Dada una lista de países, saber el total de deuda que el FMI tiene a su
-- favor.

totalDeDeuda :: [Pais] -> Number
totalDeDeuda  = sum.map deuda --ap parcial de la lista de paises, composición.


-- c. Indicar en dónde apareció cada uno de los conceptos (solo una vez) y
-- justificar qué ventaja tuvo para resolver el requerimiento.

-- 5. Debe resolver este punto con recursividad: dado un país y una lista de
-- recetas, saber si la lista de recetas está ordenada de “peor” a “mejor”, en base al
-- siguiente criterio: si aplicamos una a una cada receta, el PBI del país va de menor
-- a mayor. Recordamos que el Producto Bruto Interno surge de multiplicar el
-- ingreso per cápita por la población activa (privada y pública).

-- estaOrdenadaDePeorAMayor unPais (x) = True
-- estaOrdenadaDePeorAMayor unPais (x:y:xs) 
--         | productoBrutoInterno $aplicarUnaReceta unPais x < productoBrutoInterno $aplicarUnaReceta unPais y = True
--         | otherwise = False  

-- aplicarUnaReceta :: Pais -> Receta -> Pais
-- aplicarUnaReceta pais receta = foldl aplicarUnaEstrategiaDelFmi pais receta

-- aplicarUnaEstrategiaDelFmi unPais estrategia = estrategia unPais

-- productoBrutoInterno :: Pais -> Number
-- productoBrutoInterno unPais = ingresoPerCapita unPais + sectorPrivado unPais + sectorPrivado unPais
