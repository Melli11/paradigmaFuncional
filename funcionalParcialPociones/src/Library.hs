module Library where
import PdePreludat

data Persona = Persona {
suerte :: Number,
inteligencia :: Number,
fuerza :: Number,
nombrePersona :: String
} deriving (Show,Eq)

data Pocion = Pocion {
  nombrePocion :: String,
  ingredientes :: [Ingrediente]
}

type Efecto = Persona -> Persona

data Ingrediente = Ingrediente {
  nombreIngrediente :: String,
  efectos :: [Efecto]
}

nombresDeIngredientesProhibidos = [
 "sangre de unicornio",
 "veneno de basilisco",
 "patas de cabra",
 "efedrina"]

maximoSegun :: Ord b => (a -> b) -> [a] -> a
maximoSegun _ [ x ] = x
maximoSegun  f ( x : y : xs)
  | f x > f y = maximoSegun f (x:xs)
  | otherwise = maximoSegun f (y:xs)

--PERSONA DE PRUEBA
alvaro = Persona {
suerte = 0,
inteligencia = 70,
fuerza = 200,
nombrePersona = "Pechan"
} 

{-  1)  Dada una persona definir las siguientes funciones para cuantificar
sus niveles de suerte, inteligencia y fuerza sin repetir código:
-}
{-  1a) sumaDeNiveles que suma todos sus niveles -}
--sumaDeNiveles :: Persona -> Number -> Number

--Sol menos demostrativo
sumaDeNiveles :: Persona -> Number 
sumaDeNiveles persona =  suerte persona +  inteligencia persona +  fuerza persona


--Sol ideal
niveles :: Persona -> [Number]
niveles persona = [suerte persona, inteligencia persona, fuerza persona]

sumaDeNiveles' = sum.niveles

{-  1b) diferenciaDeNiveles es la diferencia entre el nivel más alto y más bajo.-}
--Sol menos demostrativo
diferenciaDeNiveles :: Persona -> Number
diferenciaDeNiveles persona = mayorNivel persona - menorNivel persona 
mayorNivel persona =  suerte persona `max`  inteligencia persona `max`  fuerza persona 
menorNivel persona =  suerte persona  `min` inteligencia persona `min` fuerza persona 

--Sol ideal
diferenciaDeNiveles' persona = mayorNivel' persona - menorNivel' persona
mayorNivel' = maximum.niveles
menorNivel' = minimum.niveles

{-  1c) nivelesMayoresA n, que indica la cantidad de niveles de la persona que están por encima del valor dado. -}

nivelesMayoresA n persona= length.filter (>n).niveles $persona
nivelesMayoresA' n = length.filter (>n).niveles 
{-
comentario
(>n) está aplicado parcialmente, es decir se encuentra esperando otro elemento comparable genérico,
que será buscado en la lista de niveles.
-}

{-
2) Definir la función efectosDePocion que dada una poción
 devuelve una lista con los efectos de todos sus ingredientes.
-}

efectosDePocion :: Pocion -> [Efecto]
efectosDePocion pocion = concat.map (efectos) $ingredientes pocion --sin app parcial
efectosDePocion' = concat.map (efectos).ingredientes  -- con app parcial y composicion 

{-
3) Dada una lista de pociones, consultar:
3a) Los nombres de las pociones hardcore, que son las que tienen al menos 4 efectos.
-}

pocionesHardcore :: [Pocion] -> [String]
pocionesHardcore = map(nombrePocion).filter ((>4).length.efectosDePocion)

{-
3b) La cantidad de pociones prohibidas, que son aquellas que tienen algún ingrediente
cuyo nombre figura en la lista de ingredientes prohibidos.
-}

pocionesProhibidas :: [Pocion] -> Number
pocionesProhibidas = length.filter (esProhibido)

{- NOTA:  En la condicion del filter estoy particularizando la condicion que
deben cumplir los elementos de la lista, en este caso la lista son pociones
 entonces el nombre correcto de la función  será esProhibido  
 y su tipo será de algo que va desde : Pocion -> Bool
y NO  : esIngredienteProhibido :: Ingrediente -> Bool
-}

--solucion oficial
esProhibido :: Pocion -> Bool
esProhibido = any (flip elem nombresDeIngredientesProhibidos.nombreIngrediente  ) . ingredientes


--solución en cascada

esProhibido' :: Pocion -> Bool
esProhibido' pocion = any (figuraEnlaLista') $ ingredientes pocion  

--solución usando app parcial y composición...bien 
figuraEnlaLista' :: Ingrediente -> Bool
figuraEnlaLista'  = flip elem  nombresDeIngredientesProhibidos.nombreIngrediente 

--solución basica utilizando anidación de funciones no demuestro nada no recomendada
figuraEnlaLista :: Ingrediente -> Bool
figuraEnlaLista  ingrediente = elem  (nombreIngrediente ingrediente)  nombresDeIngredientesProhibidos

--solucion oficial

{-
3c) Si son todas dulces, lo cual ocurre cuando todas las pociones 
de la lista tienen algún ingrediente llamado “azúcar”.
-}

sonTodasDulces  :: [Pocion] -> Bool
sonTodasDulces  = all (tieneIngredienteAzucar)
 
tieneIngredienteAzucar :: Pocion -> Bool
tieneIngredienteAzucar  = any (==  "azucar" ) . map (nombreIngrediente) . ingredientes

--solucion oficial
sonTodasDulces'  :: [Pocion] -> Bool
sonTodasDulces' = all (any (("azucar"==) . nombreIngrediente) . ingredientes)



{-
Definir la función tomarPocion que recibe una poción y una persona, y devuelve como quedaría 
la persona después de tomar la poción. Cuando una persona toma una poción,
se aplican todos los efectos de esta última, en orden.
-}

-- fold de una

tomarPocion :: Pocion -> Efecto
tomarPocion pocion personaInicial = foldl(\persona efecto -> efecto persona) personaInicial (efectosDePocion pocion ) --sol mas entendible
tomarPocion' pocion personaInicial = (foldl(\persona efecto -> efecto persona) personaInicial.efectosDePocion) pocion 

--tomarPocion pocion persona =  fold (aplicarUnEfecto) persona efectos.ingredientes.pocion 

--aplicarUnaPocion:: Pocion -> (Persona -> Persona)
--aplicarUnEfecto ::  Pocion -> (Persona -> Persona)
--aplicarUnaPocion unaPocion persona = persona (efectos.ingredientes.unaPocion) 

{-
Definir la función esAntidotoDe que recibe dos pociones y una persona, 
y dice si tomar la segunda poción revierte los cambios que se producen 
en la persona al tomar la primera.
-}

esAntidotoDe :: Pocion -> Pocion -> Persona -> Bool
esAntidotoDe pocionA pocionB persona = (== persona).tomarPocion pocionB.tomarPocion pocionA $persona 
esAntidotoDe' pocionA pocionB persona = ((== persona).tomarPocion pocionB.tomarPocion pocionA )persona 

{-
Definir la función personaMasAfectada que recibe una poción, una función cuantificadora 
(es decir, una función que dada una persona retorna un número) y una lista de personas,
y devuelve a la persona de la lista que hace máxima el valor del cuantificador.
Mostrar un ejemplo de uso utilizando los cuantificadores definidos en el punto 1.
-}


{-
maximoSegun :: Ord b => (a -> b) -> [a] -> a
maximoSegun _ [ x ] = x
maximoSegun  f ( x : y : xs)
  | f x > f y = maximoSegun f (x:xs)
  | otherwise = maximoSegun f (y:xs)
-}

personaMasAfectada  :: Pocion -> (Persona -> Number) -> [Persona] -> Persona  
personaMasAfectada pocion f listaDePersonas =  maximoSegun (f . tomarPocion pocion) listaDePersonas


personaMasAfectada'  :: Pocion -> (Persona -> Number) -> ([Persona] -> Persona)  
personaMasAfectada' pocion f =  maximoSegun (f . tomarPocion pocion) 

--Ejemplo de uso
--EJ1:
--personaMasAfectada (Pocion "placebo" []) sumaDeNiveles [ConjuntoDePersonas]

--EJ2:
--personaMasAfectada (Pocion "placebo" []) (nivelesMayoresA 10) []