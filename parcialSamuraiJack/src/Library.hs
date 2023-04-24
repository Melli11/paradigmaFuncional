module Library where
import PdePreludat
import GHC.Num (Num, subtract)
import Data.Type.Coercion (TestCoercion)

doble :: Number -> Number
doble numero = numero + numero


data Elemento = UnElemento {
    tipo :: String,
    ataque :: Personaje-> Personaje,
    defensa :: Personaje -> Personaje
    } deriving (Show )

data Personaje = UnPersonaje {
    nombre :: String,
    salud :: Number,
    elementos :: [Elemento],
    anioPresente :: Number
    } deriving (Show )

-- Empecemos por algunas transformaciones básicas:
-- a. mandarAlAnio: lleva al personaje al año indicado.

mandarAlAnio :: Number -> Personaje -> Personaje
mandarAlAnio  año unPersonaje = unPersonaje { anioPresente = año}

-- b. meditar: le agrega la mitad del valor que tiene a la salud del personaje.

meditar :: Personaje -> Personaje
meditar unPersonaje  = modificarSaludDelPersonaje (salud unPersonaje *0.5) unPersonaje

modificarSaludDelPersonaje :: Number -> Personaje -> Personaje
modificarSaludDelPersonaje cantidad unPersonaje = unPersonaje { salud = salud unPersonaje + cantidad}

-- c. causarDanio: le baja a un personaje una cantidad de salud dada.
-- Hay que tener en cuenta al modificar la salud de un personaje que ésta nunca puede
-- quedar menor a 0.

causarDanio :: Number -> Personaje -> Personaje
causarDanio cantidad unPersonaje
    |   laSaludSeráMayorA0 cantidad unPersonaje =  modificarSaludDelPersonaje (negate cantidad) unPersonaje
    |   otherwise = unPersonaje
-- Importante:no repetir lógica.

laSaludSeráMayorA0 :: Number -> Personaje -> Bool
laSaludSeráMayorA0 cantidad unPersonaje = salud unPersonaje - cantidad > 0

--  Queremos poder obtener algo de información extra sobre los personajes. Definir las
-- siguientes funciones:

-- data Elemento = UnElemento {
--     tipo :: String,
--     ataque :: Personaje-> Personaje,
--     defensa :: Personaje -> Personaje
--     }


-- data Personaje = UnPersonaje {
--     nombre :: String,
--     salud :: Number,
--     elementos :: [Elemento],
--     anioPresente :: Number
--     }
-- a. esMalvado, que retorna verdadero si alguno de los elementos que tiene el personaje
-- en cuestión es de tipo “Maldad”.

esMalvado :: Personaje -> Bool
esMalvado  = elem "Maldad" . listaDeTiposDeElementos

listaDeTiposDeElementos :: Personaje -> [String]
listaDeTiposDeElementos unPersonaje = map tipo $elementos unPersonaje

-- b. danioQueProduce :: Personaje -> Elemento -> Float, que retorne la diferencia entre
-- la salud inicial del personaje y la salud del personaje luego de usar el ataque del
-- elemento sobre él.

danioQueProduce :: Personaje -> Elemento -> Number
danioQueProduce unPersonaje elemento = diferenciaEntre  (salud unPersonaje)  $ salud (ataque elemento unPersonaje)

diferenciaEntre :: Number -> Number -> Number
diferenciaEntre saludInicial saludFinal =  saludInicial - saludFinal

realizarAtaqueSobrePersonaje :: Elemento -> Personaje -> Personaje
realizarAtaqueSobrePersonaje = ataque

-- -- Test 
-- *Spec Library Spec> realizarAtaqueSobrePJ bolaDeFuego darthVader 
-- UnPersonaje {nombre = "Darth Vader", salud = 90, elementos = [], anioPresente = 2022}
-- *Spec Library Spec> darthVader 
-- UnPersonaje {nombre = "Darth Vader", salud = 100, elementos = [], anioPresente = 2022


-- c.enemigosMortalesque dado un personaje y una lista de enemigos, devuelve la lista
-- de los enemigos que pueden llegar a matarlo con un solo elemento. Esto sucede si
-- luego de aplicar el efecto de ataque del elemento, el personaje queda con salud igual a
-- 0.

enemigosMortalesque :: Personaje -> [Personaje] -> [Personaje]
enemigosMortalesque personaje = filter (puedeLLegarAMatarloConAlgunElemento personaje )

puedeLLegarAMatarloConAlgunElemento :: Personaje -> Personaje -> Bool
puedeLLegarAMatarloConAlgunElemento unPersonaje enemigo = any ((==0). danioQueProduce unPersonaje) $ elementos enemigo

ataqueDeUn :: Personaje -> [Personaje -> Personaje]
ataqueDeUn personaje = map ataque $elementos personaje

-- 3. Definir los siguientes personajes y elementos:

-- a. Definir concentracion de modo que se pueda obtener un elemento cuyo efecto
-- defensivo sea aplicar meditar tantas veces como el nivel de concentración indicado y
-- cuyo tipo sea "Magia".

-- aux
-- meditar :: Personaje -> Personaje
-- meditar unPersonaje  = modificarSaludDelPersonaje (salud unPersonaje *0.5) unPersonaje



-- concentracion cantidadDeVeces = 

-- b. Definir esbirrosMalvados que recibe una cantidad y retorna una lista con esa cantidad
-- de esbirros (que son elementos de tipo “Maldad” cuyo efecto ofensivo es causar un
-- punto de daño)

data Esbirro = Elemento String Number




-- agregarALista 0 [] = [] 
-- agregarALista unElementonVeces lista 
--     | unElementonVeces == 1 = [unElementonVeces]
--     | otherwise = unElementonVeces: agregarALista (unElementonVeces) lista 

-- VARIABLES AUXILIARES

bolaDeFuego :: Elemento
bolaDeFuego = UnElemento "Fuego" (causarDanio 10) (mandarAlAnio 0)

darthVader :: Personaje
darthVader = UnPersonaje "Darth Vader" 100 [] 2022