{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use section" #-}

module Library where
import PdePreludat

-- 1. Modelar a los animales: escribir un sinónimo de tipo y definir algunos ejemplos de animales
-- como constantes. De un animal se sabe su coeficiente intelectual (un número), su especie (un
-- string) y sus capacidades (strings).

data Animal = Animal {
    coeficienteIntelectual :: Number,
    especie :: String,
    capacidades :: [String]
}deriving(Show ,Eq)


-- 2. Transformar a un animal de laboratorio:
-- ● inteligenciaSuperior: Incrementa en n unidades su coeficiente intelectual

inteligenciaSuperior :: Number -> Animal -> Animal
inteligenciaSuperior cantidadUnidades unAnimal  = unAnimal { coeficienteIntelectual = coeficienteIntelectual unAnimal + cantidadUnidades}

-- ● pinkificar: quitarle todas las habilidades que tenía
pinkificar :: Animal -> Animal
pinkificar unAnimal = unAnimal { capacidades = []}

-- ● superpoderes:  le da habilidades nuevas
    -- ○ En caso de ser un elefante: le da la habilidad “no tenerle miedo a los ratones”
    -- ○ En caso de ser un ratón con coeficiente intelectual mayor a 100: le agrega la
    -- habilidad de “hablar”.
    -- ○ Si no, lo deja como está.

superpoderes :: Animal -> Animal
superpoderes (Animal iq "elefante" capacidades ) = Animal iq "elefante" ("no tenerle miedo a los ratones": capacidades)

superpoderes (Animal iq "raton" capacidades )
        |   siTieneIQMayorA 100 (Animal iq "raton" capacidades ) = Animal iq "raton" ("hablar": capacidades )
        |   otherwise = Animal iq "raton" capacidades


superpoderes (Animal iq especie capacidades ) = Animal iq especie capacidades
-- superpoderes ratonCrack Animal {coeficienteIntelectual = 101, especie = "raton", capacidades = ["hablar","Romper las bolas"]}
-- superpoderes ratonFalladito  Animal {coeficienteIntelectual = 99, especie = "raton", capacidades = ["Fallado nomas"]}

siTieneIQMayorA :: Number -> Animal -> Bool
siTieneIQMayorA iQ = (>iQ).coeficienteIntelectual


-- 3. Los científicos muchas veces desean saber si un animal cumple ciertas propiedades, porque
-- luego las usan como criterio de éxito de una transformación. Desarrollar los siguientes criterios:

-- ● antropomórfico: si tiene la habilidad de hablar y su coeficiente es mayor a 60

antropomórfico :: Animal -> Bool
antropomórfico unAnimal  = siTieneIQMayorA  60 unAnimal && tieneLaHabilidad "hablar" unAnimal --Se puede mejorar???

tieneLaHabilidad :: String -> Animal -> Bool
tieneLaHabilidad habilidad = elem habilidad . capacidades

-- noTanCuerdo: si tiene más de dos habilidades puede hacer sonidos pinkiescos. Hacer una
-- función pinkiesco, que significa que la habilidad empieza con “hacer ”, y luego va seguido
-- de una palabra "pinkiesca", es decir, con 4 letras o menos y al menos una vocal

noTanCuerdo :: Animal -> Bool
noTanCuerdo unAnimal
       |   tieneMasDe_N_habilidades 2 unAnimal =  any pinkiesco $ capacidades unAnimal
       |   otherwise = False

tieneMasDe_N_habilidades :: Number -> Animal -> Bool
tieneMasDe_N_habilidades cantidad = (>cantidad ).length.capacidades

pinkiesco :: String -> Bool
pinkiesco palabra = empiezaConPalabraHAcer (take 6 palabra)  && palabraPinkiesca (drop 6 palabra)

empiezaConPalabraHAcer ::String -> Bool
empiezaConPalabraHAcer palabra =  palabra == "hacer "

palabraPinkiesca :: String -> Bool
palabraPinkiesca  palabra = ((<=4) . length) palabra && tieneAlMenosUnaVocal palabra --Se puede mejorar???

esUnaVocal :: Char -> Bool
esUnaVocal letra =  letra == 'a' || letra == 'e' || letra == 'i' || letra == 'o' || letra == 'u'

tieneAlMenosUnaVocal :: String -> Bool
tieneAlMenosUnaVocal = any esUnaVocal

-- 4. Los científicos construyen experimentos: un experimento se compone de un conjunto de
-- transformaciones sobre un animal, y un criterio de éxito. Se pide:
-- ● Modelar a los experimentos: dar un sinónimo de tipo.
-- ● Desarollar experimentoExitoso: Dado un experimento y un animal, indica si al aplicar
-- sucesivamente todas las transformaciones se cumple el criterio de éxito.

type Transformar_Animal = Animal -> Animal

data Experimento = Experimento {
    transformaciones :: [Transformar_Animal],
    criterioDeExito :: Animal -> Bool
}

experimentoExitoso :: Animal -> Experimento -> Bool
experimentoExitoso unAnimal unExperimento = criterioDeExito unExperimento $ aplicarUnExperimento unAnimal unExperimento

aplicarUnExperimento :: Animal -> Experimento -> Animal --un experimento es un conjunto de transformaciones
aplicarUnExperimento unAnimal unExperimento = foldl aplicarUnaTransformacion unAnimal (transformaciones unExperimento)

aplicarUnaTransformacion :: Animal -> Transformar_Animal -> Animal
aplicarUnaTransformacion unAnimal transformacion = transformacion  unAnimal


--5. Periódicamente, ACME pide informes sobre los experimentos realizados. Desarrollar los
-- siguientes reportes, que a partir de una lista de animales, una lista de capacidades y un
-- experimento (o una serie de transformaciones) permitan obtener:

-- 1. una lista con los coeficientes intelectuales de los animales que entre sus capacidades,
-- luego de efectuar el experimento, tengan ALGUNA de las capacidades dadas.


primerInforme_IQ :: [String] -> [Animal] -> Experimento -> [Number]
-- primerInforme_IQ  listaDeCapacidades listaAnimal  = losIQdeLosAnimales .  tienenAlgunasCapacidades listaDeCapacidades . efectuarUnExperimentoSobreListaAnimal listaAnimal 
primerInforme_IQ  listaDeCapacidades listaAnimal  = losIQdeLosAnimales .  filtrarPorCapacidades tieneAlgunaCapacidad listaDeCapacidades . efectuarUnExperimentoSobreListaAnimal listaAnimal 

-- Abstraccion para evitar la repeticion de logica : Filtrar por criterio de capacidades
filtrarPorCapacidades :: ([String] -> Animal -> Bool) -> [String] -> [Animal] -> [Animal]
filtrarPorCapacidades criterioDeBusqueda lasCapacidades listadoDeAnimales = filter (criterioDeBusqueda lasCapacidades) listadoDeAnimales

-- F auxiliar 
efectuarUnExperimentoSobreListaAnimal :: [Animal] -> Experimento -> [Animal]
efectuarUnExperimentoSobreListaAnimal listadoDeAnimales experimento = map (flip aplicarUnExperimento experimento) listadoDeAnimales

-- F auxiliar 
losIQdeLosAnimales :: [Animal] -> [Number] 
losIQdeLosAnimales = map coeficienteIntelectual

-- F auxiliar 

tieneAlgunaCapacidad :: [String] -> Animal -> Bool
tieneAlgunaCapacidad listaDeCapacidades animal = any (flip tieneLaHabilidad animal) listaDeCapacidades

-- AUX
-- tieneLaHabilidad :: String -> Animal -> Bool
-- tieneLaHabilidad habilidad = elem habilidad . capacidades

-- Prueba En Consola
-- primerInforme_IQ listadoDeCapacidades2 listadoDeAnimales unExperimento2
-- [40,71]

-- 2. una lista con las especie de los animales que, luego de efectuar el experimento, tengan
-- entre sus capacidades todas las capacidades dadas.

-- segundoInforme_Especies ::  
segundoInforme_Especies :: [String] -> [Animal] -> Experimento -> [String]
segundoInforme_Especies listaDeCapacidades listaAnimal  = lasEspeciesdeLosAnimales . filtrarPorCapacidades tieneTodasLasCapacidades listaDeCapacidades . efectuarUnExperimentoSobreListaAnimal listaAnimal 

lasEspeciesdeLosAnimales :: [Animal] -> [String]
lasEspeciesdeLosAnimales = map especie

tieneTodasLasCapacidades :: [String] -> Animal -> Bool
tieneTodasLasCapacidades listaDeCapacidades animal = all (flip tieneLaHabilidad animal) listaDeCapacidades

-- Spec Library Spec> segundoInforme_Especies listadoDeCapacidades2 listadoDeAnimales unExperimento2
-- ["tigre"]

-- 3. una lista con la cantidad de capacidades de todos los animales que, luego de efectuar el
-- experimento, no tengan ninguna de las capacidades dadas.

tercerInforme_CantidadCapacidades :: [String] -> [Animal] -> Experimento -> [Number]
tercerInforme_CantidadCapacidades listaDeCapacidades listaAnimal =  laCantidadDeCapacidades .  filtrarPorCapacidades noTieneTodasLasCapacidades listaDeCapacidades . efectuarUnExperimentoSobreListaAnimal listaAnimal

laCantidadDeCapacidades :: [Animal] -> [Number]
laCantidadDeCapacidades  = map (length.capacidades)

noTieneTodasLasCapacidades :: [String] -> Animal -> Bool
noTieneTodasLasCapacidades listaDeCapacidades animal = all (flip noTieneLaHabilidad animal) listaDeCapacidades

noTieneLaHabilidad :: String -> Animal -> Bool
noTieneLaHabilidad habilidad = (== False). elem habilidad . capacidades

-- *Spec Library Spec> tercerInforme_CantidadCapacidades listadoDeCapacidades listadoDeAnimales unExperimento2
-- [2,2,2,1,3]

-- Variables Auxiliares
-- ejemplos de animales
perro :: Animal
perro = Animal 30 "perro" ["Ladrar","Jugar"]
perroParlante :: Animal
perroParlante = Animal 30 "perro" ["hablar","Jugar"]
tigre :: Animal
tigre = Animal 61 "tigre" ["Veloz","Camuflar","hablar"]

ratonCrack :: Animal
ratonCrack = Animal 101 "raton" ["Romper las bolas"]
ratonFalladito :: Animal
ratonFalladito = Animal 99 "raton" ["Fallado nomas","hablar"]
elefante :: Animal
elefante = Animal 300 "elefante" ["Inteligente"]

unExperimento :: Experimento
unExperimento = Experimento [pinkificar,inteligenciaSuperior 10,superpoderes] antropomórfico -- deja sin hablidades , incrementa 10 unidades el iq, si es elefante le otorga la habilidad "no tenerle miedo a los ratones" en caso de raton agrega habilidad de hablar

unExperimento2 :: Experimento
unExperimento2 = Experimento [inteligenciaSuperior 10] antropomórfico --  incrementa 10 unidades el iq,


otroExperimento :: Experimento
otroExperimento = Experimento [inteligenciaSuperior 10,inteligenciaSuperior 10,superpoderes] antropomórfico

ratonExperimental :: Animal
ratonExperimental = Animal 17 "raton" ["destruir el mundo","hacer planes malvados"]

listadoDeAnimales :: [Animal]
listadoDeAnimales = [perro,perroParlante,ratonExperimental,ratonCrack,tigre]

listadoDeCapacidades :: [String]
listadoDeCapacidades = ["nadar", "gritar","no hacer nada"] --no deberia retornar ningun iq

listadoDeCapacidades2 :: [String]
listadoDeCapacidades2 = ["Veloz","Camuflar","hablar"] --devuelve tigre en 2do informe