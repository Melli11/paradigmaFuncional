module Library where
import PdePreludat
{-Mi version
-- Punto 1
{-Crear el modelo necesario que mejor se adapte para la solución.
Además:
Indique el tipo de un cargo.
Indique el tipo de un impuesto
-}

type Ciudad = String
type Pais = String
type Peso = Number
type Precio = Number
type Categoria = String
data Envio = Envio {
    lugarDeOrigen :: (Ciudad,Pais),
    lugarDeDestino :: (Ciudad,Pais),
    peso :: Peso,
    precioBase :: Precio,
    categorias :: [Categoria],
    impuestos :: [Impuesto]
} deriving (Show)

{- 1er Error: Modele funciones que no me piden

--CargoCategorico si la pide.

cargoCategorico :: Envio -> [String] -> Number -> Envio   
cargoCategorico envio categoria porcentaje
    |  elem categoria categorias = envio { precioBase = precioBase envio - porcentaje }                                                     
    |  otherwise =  id


iva envio = envio { precioBase = precioBase envio + precioBase envio*0.2 }
-}
{-
multicategoria :: Envio -> Envio

multicategoria envio 
    | ((>=3).length.categorias) envio = envio { precioBase = precioBase envio + precioBase envio*0.01 }
    | otherwise = id

aduanero :: Envio -> Envio
aduanero envio 
    |  lugarDeOrigen envio /= lugarDeDestino envio =  envio { precioBase = precioBase envio + precioBase envio*0.03 }
    |  otherwise = id

impuestoExtraño :: Envio -> Envio
impuestoExtraño envio
    |  (even.precioBase) envio = envio { precioBase = precioBase envio + precioBase envio*0.1 }
    |  otherwise = id
-}
--  1.Crear el modelo necesario que mejor se adapte para la solución.Además:

--  Indique el tipo de un cargo.
--  Indique el tipo de un impuesto

type Cargo = Envio -> Envio
type Impuesto = Envio -> Envio

-- 2.Modelar con funciones constantes:   
-- 2a.Un cargo categórico de “tecnología” de 18

cargoCategorico :: Envio -> Categoria -> Number -> Envio   
cargoCategorico envio categoria porcentaje
    |  elem categoria (categorias envio) = envio { precioBase = precioBase envio - porcentaje }                                                     
    |  otherwise =  id envio


cargoTecnologico = cargoCategorico envio2a "tecnologia" 18

envio2a = Envio {
    lugarDeOrigen = ("",""),
    lugarDeDestino = ("",""),
    peso = 0,
    precioBase = 0,
    categorias = ["tecnologia"],
    impuestos = []
} 


--cargoCategorico envio2a "tecnologia" 18    
--cargoCategorico envio tecnologia 18

-- 2b.Envío con origen en Buenos Aires, Argentina y con destino Utrecht, Países Bajos, de 2kg de peso, precio base de $220, con las categorías de música y tecnología, sin impuestos.

envio2b = Envio {
    lugarDeOrigen = ("Buenos Aires","Argentina"),
    lugarDeDestino = ("Ultrecht","Paises Bajos"),
    peso = 2,
    precioBase = 220,
    categorias = ["Musica","Tecno"],
    impuestos = []
} 
-- 2c.Envío con origen California, Estados Unidos y con destino Miami, Estado Unidos, de 5kg de peso, precio base $1500, con categoría de libros, y con IVA e impuesto extraño.


envio2c = Envio {
    lugarDeOrigen = ("California","EEUU"),
    lugarDeDestino = ("Miami","EEUU"),
    peso = 5,
    precioBase = 1500,
    categorias = ["libros"],
    impuestos = []
} 

cargoporSobrePeso :: Envio -> Number -> Envio 
cargoporSobrePeso envio pesoKgMax 
    | peso envio <= pesoKgMax = id envio 
    | otherwise = envio { precioBase = precioBase envio + 80 * (pesoKgMax - peso envio ) }

cargoArbitrario :: Envio -> Envio
cargoArbitrario  envio = envio { precioBase = precioBase envio + 50 }

-}


-- Punto 1
type Ciudad = String
type Pais = String
type Lugar = (Ciudad, Pais)
type Categoria = String
type Peso = Number
type Precio = Number
type Cargo = Envio -> Envio
type Impuesto = Envio -> Precio
data Envio = 
    Envio {
        origen :: Lugar,
        destino :: Lugar,
        peso :: Peso,
        precioBase :: Precio,
        categorias :: [Categoria],
        impuestos :: [Impuesto]
    }

-- Punto 2a
cargo :: (Envio -> Bool) -> (Envio -> Number) -> Cargo
cargo criterio calculo envio 
    | not . criterio $ envio = envio
    | criterio envio = envio { precioBase = precioBase envio + calculo envio }

cargoCategorico :: Categoria -> Number -> Cargo
cargoCategorico categoria factor = cargo (elem categoria.categorias) ((factor *).precioBase)

cargoTecnologico :: Cargo
cargoTecnologico = cargo (elem "tecnología".categorias) ((*0.18).precioBase)
cargoTecnologico' = cargoCategorico "tecnología" 0.18

cargoSobrepeso :: Peso -> Cargo
cargoSobrepeso maximo = cargo ((>maximo).peso) ((*80).(flip (-) maximo).peso)
cargoSobrepeso' maximo = cargo (\ _ -> True) ((*80). max 0 .(flip (-) maximo).peso)
cargoSobrepeso'' maximo = cargo (const True) ((*80). max 0 .(flip (-) maximo).peso)

cargoArbitrario :: Cargo
cargoArbitrario = cargo (const True) (const 50)

-- Impuestos
impuesto :: (Envio -> Bool) -> (Precio -> Number) -> Impuesto
impuesto criterio calculo envio 
    | not . criterio $ envio = 0
    | criterio envio = calculo $ precioBase envio

iva = impuesto (const True) (*0.2)
multicategoria = impuesto ((>3).length.categorias) (*0.01)
aduanero = impuesto esInternacional (*0.03)
extranio = impuesto (even.precioBase) (*0.1)

-- Punto 2b
envioInternacional = Envio {
        origen = ("Buenos Aires", "Argentina"),
        destino = ("Utrecht", "Paises Bajos"),
        peso = 2,
        precioBase = 220,
        categorias = ["música","tecnología"],
        impuestos = []
}

-- Punto 2c
envioDomestico = Envio 
    ("California", "Estados Unidos") 
    ("Miami", "Estados Unidos") 
    5 
    1500 
    ["libros"]
    [iva, extranio]

-- Punto 3

-- Punto 4a
seDirige unPais = (unPais==).pais.destino
-- Punto 4b
pais = snd
esLocal envio = seDirige (pais.origen $ envio) envio
esInternacional = not.esLocal

-- Punto 6 ( se recomienda hacer :D )

-- Punto 8
whatever a b c  = c a . filter ((a==) . b)