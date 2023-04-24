module Library where
import PdePreludat
import Data.Char (toUpper)

data Color = Rojo | Verde | Amarillo | Azul deriving(Show,Eq)
data Simbolo = Reversa | Mas4 | SaltarTurno deriving (Eq, Show)

data Carta =
    CartaNumerica { numero :: Number, color :: Color } |
    CartaEspecial { simbolo :: Simbolo, color :: Color }
    deriving (Eq, Show)

-- Ejemplo de mazos y Cartas

mazo1 :: [Carta]
mazo1 = [CartaNumerica 5 Rojo,CartaNumerica 0 Azul, CartaNumerica 1 Azul, CartaNumerica 1 Verde,CartaNumerica 2 Rojo,CartaNumerica 1 Rojo]

mazo2 :: [Carta]
mazo2 = CartaEspecial Mas4 Rojo : CartaNumerica 3 Rojo : CartaNumerica 9 Verde : CartaNumerica 0 Azul : CartaEspecial Reversa Rojo : []

cartaCeroAzul :: Carta
cartaCeroAzul = CartaNumerica 0 Azul
cartaUnoVerde = CartaNumerica 1 Verde
cartaUnoRoja = CartaNumerica 1 Rojo
cartaAmarilla = CartaEspecial Reversa Rojo
cartaAmarilla2 = CartaEspecial Mas4 Rojo
cartaAmarilla3 = CartaEspecial SaltarTurno Rojo
listaNumerica = [10,10,10,10,10]

--obtenerElemento: recibe un numero y una lista, y devuelve el elemento con ese indice en la lista. Ej:
obtenerElemento :: Number -> [a] -> a
obtenerElemento 0 (primerElemento:colaDeLista) = primerElemento
obtenerElemento indice (primerElemento:colaDeLista) = obtenerElemento (indice - 1) colaDeLista

--sacarHastaEncontrar: recibe una lista de cartas y una carta, devuelve todas las cartas en la lista hasta encontrar la carta buscada. 
-- Si la carta no está, devuelve todas las cartas pasadas.

sacarHastaEncontrar :: Carta -> [Carta] -> [Carta]
sacarHastaEncontrar _ [] = []            
sacarHastaEncontrar carta ( cabezaDeListaDeCartas : colaDeCartas) 
            | siLasDosCartasSonIguales carta cabezaDeListaDeCartas = [carta]
            | not (siLasDosCartasSonIguales carta cabezaDeListaDeCartas) = cabezaDeListaDeCartas:sacarHastaEncontrar carta colaDeCartas

siLasDosCartasSonIguales :: Carta -> Carta -> Bool
siLasDosCartasSonIguales carta otraCarta = carta == otraCarta

-- lasRojas: dada una lista de cartas, retorna una lista con solo aquellas que son rojas. Ej:

lasRojas :: [Carta] -> [Carta] 
lasRojas listaDeCartas = lasQueSonDeColor Rojo listaDeCartas

--lasQueSonDeColor: dada una lista de cartas y un color, retorna una lista con solo aquellas que son de ese color. Ej:

lasQueSonDeColor :: Color -> [Carta] -> [Carta]
lasQueSonDeColor colorSeleccionado (primeraCarta:colaDeCartas)   
        |  criterioColor colorSeleccionado primeraCarta = primeraCarta: lasQueSonDeColor colorSeleccionado colaDeCartas
        |  otherwise = lasQueSonDeColor colorSeleccionado colaDeCartas

lasQueSonDeColor _ [] = []

criterioColor :: Color -> Carta -> Bool
criterioColor colorSeleccionado carta = color carta == colorSeleccionado

--lasFiguras: dada una lista de cartas, retorna una lista con solo aquellas que tengan figuras. Ej:

lasFiguras ::  [Carta] -> [Carta]
lasFiguras  (primeraCarta:colaDeCartas)   
        |  criterioFigura primeraCarta = primeraCarta: lasFiguras colaDeCartas
        |  otherwise = lasFiguras colaDeCartas

lasFiguras [] = []

criterioFigura :: Carta -> Bool
criterioFigura  (CartaEspecial _ _) = True 
criterioFigura  (CartaNumerica _ _) = False 
-- completar de tal manera que cuando se pregunte por el simbolo sea true y cuando se pregunte por el numero sea false.

sumatoria :: [Number] -> Number
sumatoria [] = 0
sumatoria (primerElemento:restoDeElementos) = primerElemento + sumatoria restoDeElementos

