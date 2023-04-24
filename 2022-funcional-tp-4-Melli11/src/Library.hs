module Library where
import PdePreludat

data Animal = Animal {
    energia :: Number,
    tipo :: Tipo,
    peso :: Number
} deriving(Show, Eq)

data Tipo = Volador | Terrestre | Acuatico deriving(Show, Eq)

-- Parte 1: Animales
tiburon = Animal 100 Acuatico 100
tigre = Animal 5 Terrestre 120
lechuza = Animal 40 Volador 10
listaDeAnimales = [Animal 10 Volador 100, Animal 20 Terrestre 200, Animal 30 Acuatico 100 ]

--losDeTipo :: ( Tipo -> Bool) -> [Animal] -> [Animal]
losDeTipo :: Tipo -> [Animal] -> [Animal]
losDeTipo unTipo listaDeAnimales = filtrar (\animal -> tipo animal == unTipo) listaDeAnimales

-- animalesHambrientos: dada una lista de animales, nos devuelve solo aquellos que tienen hambre.
--  Que un animal tenga hambre significa que su energía es menor a 10.

animalesHambrientos' :: [Animal] -> [Animal]
animalesHambrientos' listaDeAnimales = filtrar (\animal -> energia animal < 10) listaDeAnimales 

animalesHambrientos :: [Animal] -> [Animal]
animalesHambrientos listaDeAnimales = filtrar (siElAnimalEstaHabriento) listaDeAnimales 

siElAnimalEstaHabriento :: Animal -> Bool
siElAnimalEstaHabriento animal = energia animal < 10

filtrar :: (a -> Bool) -> [a] -> [a]
filtrar _ [] = []
filtrar condicion (x : xs)
     | condicion x = x : filtrar condicion xs
     | otherwise = filtrar condicion xs

aplicarATodos ::  (t -> a) -> [t] -> [a]
aplicarATodos funcion [] = []
aplicarATodos funcion (elemento : elementos) =
    (funcion elemento : aplicarATodos funcion elementos)

-- --1raForma
entrenar :: Animal -> Animal
entrenar animal
        |  tipo animal == Terrestre = animal { energia  = operarAtributoCon (-) (energia animal) 5,
                                             peso  = operarAtributoCon (-) (peso animal) 5}
        |  tipo animal == Volador = animal {peso = operarAtributoCon (-) (peso animal) 3}
        |  otherwise = animal

-- --2daForma
entrenar' :: Animal -> Animal
entrenar' (Animal energiaAnimal Terrestre pesoAnimal) = Animal (operarAtributoCon (-) energiaAnimal 5) Terrestre (operarAtributoCon (-) pesoAnimal 5)  
entrenar' (Animal energiaAnimal Volador pesoAnimal) = Animal energiaAnimal Volador (operarAtributoCon (-)  pesoAnimal 3)  
entrenar' (Animal energiaAnimal Acuatico pesoAnimal) = Animal energiaAnimal Acuatico pesoAnimal   

-- -- Parte 2: Alimentos y entrenamientos

-- implementar estos alimentos:
-- bayas aumenta la energia en 5 y el peso en 0.1
-- carne aumenta la energia en 20 y el peso en 2

operarAtributoCon :: (Number -> Number -> Number) -> Number -> Number -> Number 
operarAtributoCon operacion animalAtributo cantidad = operacion animalAtributo  cantidad

type Alimento = Animal -> Animal

baya :: Alimento  
baya animal = animal {energia = operarAtributoCon (+) (energia animal) 5, 
                        peso = operarAtributoCon (+) (peso animal) 0.1 }
carne :: Alimento 
carne animal = animal {energia = operarAtributoCon (+) (energia animal) 20, 
                        peso = operarAtributoCon (+) (peso animal) 2}

-- alimentarATodos
alimentarATodos :: Alimento -> [Animal] -> [Animal]                 
alimentarATodos alimento listaDeAnimales =   aplicarATodos alimento listaDeAnimales

aplicarItinerario :: Animal ->[Alimento] -> Animal
aplicarItinerario animal itinerario = foldl (\animal entrenamientoComida ->entrenamientoComida animal ) animal itinerario 

aplicarItinerario' :: Animal ->[Alimento] -> Animal
aplicarItinerario' animal itinerario = foldl (aplicarUnItinerario ) animal itinerario 

aplicarUnItinerario:: Animal -> Alimento -> Animal
aplicarUnItinerario  animal entrenamientoOcomida = entrenamientoOcomida animal 

-- -- Parte 3: Nuestras propias funciones de orden superior

-- mapTupla = 
mapTupla :: (a -> b) -> (a, a) -> (b, b)
mapTupla funcion (primerElemento,segundoElemento) = (funcion primerElemento,funcion segundoElemento) 

-- menorSegun
menorSegun :: Ord a => (p -> a) -> p -> p -> p
menorSegun funcion primerParametro segundoParametro
        | funcion primerParametro <= funcion segundoParametro = primerParametro
        | otherwise = segundoParametro

-- minimoSegun
minimoSegun :: Ord a => (b -> a) -> [b] -> b
minimoSegun funcion lista = foldl (menorSegun funcion) (head lista) lista

-- aplicarVeces
siguiente n = n + 1

-- 1ra Forma
--no se aclara el indice 0
aplicarVeces :: Number -> (t -> t) -> t -> t
aplicarVeces indice funcion parametro 
        | indice == 1  = funcion parametro
        | indice > 1 = aplicarVeces (indice-1) funcion (funcion parametro)
-- 2da Forma
-- no se aclara el indice 0
aplicarVeces':: Number -> (t -> t) -> t -> t
aplicarVeces' 1 funcion parametro = funcion parametro  
aplicarVeces' indice funcion parametro =  aplicarVeces' ( indice -1) funcion ( funcion parametro)


-- agregarALista :: Number -> a -> [a]
-- agregarALista 0 elemento  = []
-- agregarALista 1 elemento  = [elemento]
-- agregarALista cantidadDeVeces elemento = elemento: agregarALista (cantidadDeVeces-1) elemento 

-- MEJORADA
replicar :: Number -> a -> [a]
replicar 0 elemento = []
replicar 1 elemento = [elemento]
replicar cantidadDeVeces elemento = aplicarVeces (cantidadDeVeces-1) (++ [elemento]) [elemento]

-- -- Parte 4. Bonus: combinando funciones

-- valor |> funcion = implementame

-- esVocal = implementame

-- saltoDeLinea :: Char
-- saltoDeLinea = '\n'

-- primeraLinea = implementame

-- lasVocales = implementame

-- -- definir usando |> para combinar las funciones
-- contarVocalesDeLaPrimeraLinea = implementame
