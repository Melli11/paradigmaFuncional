module Library where
import PdePreludat

{-Con PdePreludat-}
data Accion = Accion {
   idAccion :: Simbolo,
   precios :: [Number]
} deriving (Show)


{-Generar los tipos y estructuras necesarios para modelar los usuarios y sus títulos. Nota: No usar nombres de componentes repetidos en las distintas estructuras.-}

{-También tendremos usuarios, cada uno tendrá una cartera donde se encontrará 
el efectivo que tiene y una lista de títulos de acciones, que contienen el símbolo, la cantidad y precio al cual compró las acciones.-}

type Simbolo = String
type Cantidad = Number
type Precio = Number

--type Titulos = (Simbolo,Cantidad,Precio)
data Titulo = Titulo{
    simbolo :: Simbolo,
    cantidad :: Cantidad,
    precioCompra :: Precio 
} deriving (Show)

data Usuario = Usuario {
    efectivo :: Number,
    titulosDeAcciones :: [Titulo]
} deriving (Show)

{- USUARIOS DE EJEMPLO-}
{-
--Con Tupla
jaimito = Usuario {
    efectivo = 500,
    titulosDeAcciones = [("MCD",10,50),("IBO",20,100),("YPF",5,25),("YPL",5,25)]
}
-}

--Con Data
jaimito = Usuario {
    efectivo = 500,
    titulosDeAcciones =  [Titulo "MCD" 10 50, Titulo "CDM" 20 35, Titulo "MXF" 10 100, Titulo "MXF" 10 100]
}

{-

Implementar las funciones:

i)mapCondicional: Similar al map, recibe una función de transformación, pero además una condición, 
y la transformación de cada elemento se realiza únicamente si ese elemento cumple la condición dada.
También recibe la lista a mapear, claro.
Se espera que la lista resultante tenga la misma cantidad de elementos que la original.
Escribir en un comentario: ¿de qué tipo tiene que ser la transformación para que esta función tenga sentido? ¿Por qué?
-}

-- VER ELEMENTOS EQUIPARABLES (a -> a)
mapCondicional :: (a -> a) -> (a -> Bool) -> [a] -> [a]
mapCondicional transformacion condicion lista = map (funcionDeTransformacion transformacion condicion) lista

funcionDeTransformacion transformacion condicion elemento 
        | condicion elemento = transformacion elemento 
        | otherwise = elemento  

{-
ii)encontrar: Determina el primer elemento en una lista que cumple una condición.
-}

encontrar' :: [a] -> (a -> Bool) -> a
encontrar' condicion = head .  flip filter (condicion)

encontrar ::  (a -> Bool) -> [a] -> a
encontrar condicion = head .  filter (condicion)  

{-
iii)cuantasTieneDe: Dado un símbolo y un usuario, nos indica la cantidad de acciones que tiene ese usuario en sus títulos.
-}


{-EXPLICACION

encontrar: Determina el primer elemento en una lista que cumple una condición.
en el argumento de encontrar, estoy parandome sobre los titulos del usuario, y obtengo
aquellos que recibe como parametro en la función cuantasTieneDe, para ello
armo la siguiente composición ((idSimbolo ==) . simbolo) es una comparacion de dos strings, y como resultado 
obtengo un TITULO a ello le vuelvo a aplicar la función cantidad que es parte de su definición (Titulo es un data) 
y obtengo la cantidad.

-}
cuantasTieneDe' :: Simbolo -> Usuario -> Cantidad     
cuantasTieneDe' idSimbolo usuario = cantidad . encontrar ((idSimbolo ==) . simbolo) $titulosDeAcciones  usuario   

{-
Implementar las funciones:
3)
Crear una función nuevoPrecioAccion que reciba una acción y un nuevo precio y devuelva la acción con el precio actualizado.
-}


nuevoPrecioAccion ::  Number -> Accion -> Accion
nuevoPrecioAccion  nuevoPrecio accion = accion { precios = nuevoPrecio :precios accion}

{-
Crear una función nuevoPrecio que reciba una lista de acciones, un símbolo de acción a actualizar y un nuevo precio 
y devuelva la lista de acciones con la acción a actualizar modificada.
-}

--mapCondicional :: (a -> a) -> (a -> Bool) -> [a] -> [a]
--mapCondicional ftransformacion condicion listaDeelemento

nuevoPrecio :: [Accion] -> Simbolo -> Number -> [Accion]
nuevoPrecio acciones simboloAccion precio = mapCondicional (nuevoPrecioAccion precio) ((==simboloAccion) .idAccion) acciones

{-
Crear una función precioActual que, dada una acción, nos diga su precio actual.
-}

precioActual :: Accion -> Number
precioActual = head.precios

{-
4)
Se sabe que a los usuarios les gusta saber su situación actual con un sólo número.
Queremos una función estadoActual que reciba un usuario y una lista de acciones 
y devuelva la diferencia entre los precios de compra y los precios actuales para saber si está perdiendo o ganando.
-}

{-data Accion = Accion 
    {idAccion :: Simbolo,
    precios :: [Number]}

data Usuario = Usuario {
    efectivo :: Number,
    titulosDeAcciones :: [Titulo]
} deriving (Show)

-}

--Punto 4: estadoActual

--version 1:
-- Soporta que el usuario haya comprado mas de una vez el título a precios distintos
estadoActual listaDeAcciones =
    sum.map (\titulo ->(precioAccion titulo - precio titulo) * cantidad titulo) . listaDeTitulos 
        where precioAccion titulo = precioActual .encontrar ((==simboloAccion titulo).simbolo) $listaDeAcciones 

--version 2:
-- Hace lo mismo que la anterior pero con 2 definiciones locales en vez de 1 y 1 lambda
estadoActual' usuario =
    sum.map diferenciaPrecio
        where diferenciaPrecio accion =(precioActual accion -precio (titulo accion)) *cantidad (titulo accion)
            titulo accion = encontrar (( ==simbolo accion) .simboloAccion ) $listaDeTitulos usuario



{-
5)
Algunas acciones regularmente hacen pagos a sus accionistas para agradecer que todavía tengan sus acciones. 
Hacer una función pagarDividendos que reciba una lista de usuarios, un símbolo y la cantidad de dividendos 
que se da por acción y devuelva la lista de usuarios con el efectivo actualizado.
-}

pagarDividendos :: [Usuario] -> Simbolo -> Number -> [Usuario]
pagarDividendos usuarios simbolo dividendosCantidad = 

{-
6)
Como pequeño regalo, y para que no dejen de usar la plataforma, le queremos dar $1.000 a aquellos usuarios a los que no les está yendo bien en un momento dado.
Un usuario al que no le va bien es uno en el que su situación actual (punto 4) da una pérdida mayor a $50.000.
Hacer una función rescateFinanciero que reciba una lista de acciones y una lista de usuarios, y devuelva a los usuarios
que cumplan el requerimiento con $1.000 más.
-}

{-
7)
Crear una función venta que reciba un usuario, una acción (no un símbolo) y una cantidad.
 Debe devolver al usuario con la transacción hecha. La transacción bajaría la cantidad de acciones y subiría el efectivo en (precioActual * cantidadAccionesVendidas).
-}

{-
8)
Queremos mostrar la acción que más haya convenido comprar, para eso necesitamos de algunas funciones.
i) Una función porcentajeDeGanancia que reciba una acción y devuelva el porcentaje que subió desde su primer medición hasta su precio actual (sería: precioActual * 100 / primerMedición)
-}

{-
ii) Una función mayorGanancia que, dadas dos acciones, devuelva la acción con el porcentaje de suba más grande.
}-

{-
iii) Una función laMejorAccion que dada una lista de acciones devuelva la acción con el mayor porcentaje de suba.  Nota: En este punto, se puede usar recursividad.
-}

9)
Explicar y justificar la inferencia del tipo de la siguiente función: Nota: No se pide sólo indicar el tipo. Se pide explicar de dónde surge cada uno de los tipos inferidos (parámetros y resultado).

funcionQueNoDebeSerNombrada x y = (>= x) . foldr (flip y 10) x

-}