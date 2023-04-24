module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | Papas | PatiVegano | PanIntegral
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente Papas = 10
precioIngrediente PatiVegano = 10
precioIngrediente PanIntegral = 3

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

precio :: Hamburguesa -> Number
precio hamburguesa = precioBase hamburguesa + precioTotal (ingredientes hamburguesa)

precioTotal :: [Ingrediente] -> Number
-- precioTotal ingredientes = sum (map precioIngrediente ingredientes)
precioTotal = sum . map precioIngrediente -- esta bien

-- precioTotal ingredientes = sum . map precioIngrediente $ ingredientes -- esta bien
-- haskell lo va a interpretar como:
-- (sum . map precioIngrediente) $ ingredientes

-- precioTotal ingredientes = sum . map precioIngrediente ingredientes -- esta mal
-- haskell lo va a interpretar como
-- sum . (map precioIngrediente ingredientes) 

agrandar :: Hamburguesa -> Hamburguesa
agrandar hamburguesa = agregarIngrediente (tipoDePati hamburguesa) hamburguesa

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingrediente hamburguesa =
    Hamburguesa (precioBase hamburguesa) (ingrediente : ingredientes hamburguesa)

tipoDePati hamburguesa = primeroQueSeaTipoDePati (ingredientes hamburguesa)

esTipoDePati ingrediente = ingrediente == Pollo || ingrediente == Carne

primeroQueSeaTipoDePati ingredientes = head (filter esTipoDePati ingredientes)

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcentaje hamburguesa =
    Hamburguesa (precioBase hamburguesa - precioBase hamburguesa * (porcentaje / 100))
                (ingredientes hamburguesa)

-- cuartoDeLibra es una hamburguesa de pan, carne, cheddar, pan y el precio base es de 20.

cuartoDeLibra :: Hamburguesa
cuartoDeLibra = Hamburguesa 20 [Pan, Carne, Cheddar, Pan]


-- la pdepBurguer que es un cuarto de libra agrandado 2 veces con panceta, cheddar y 20% de descuento. Su precio final deberia ser 110
pdepBurger :: Hamburguesa
pdepBurger =
    (descuento 20 . agregarIngrediente Cheddar . agregarIngrediente Panceta . agrandar . agrandar) cuartoDeLibra

-- pdepBurger = descuento 20 (agregarIngrediente Cheddar (agregarIngrediente Panceta (agrandar (agrandar cuartoDeLibra))))

-- PARTE 2: Algunas hamburguesas más
-- dobleCuarto = es un cuarto de libra con carne y cheddar. El precio final deberia ser 84.

dobleCuarto :: Hamburguesa
dobleCuarto = (agregarIngrediente Carne . agregarIngrediente Cheddar) cuartoDeLibra

-- bigPdep = es un doble cuarto con curry. El precio final deberia ser 89.
bigPdep :: Hamburguesa
bigPdep = agregarIngrediente Curry  dobleCuarto
bigPdep' :: Hamburguesa
bigPdep' = (agregarIngrediente Curry . agregarIngrediente Carne . agregarIngrediente Cheddar) cuartoDeLibra --medio ladri pero no se me ocurria otra forma

-- delDia = es una promo que dada una hamburguesa, le agrega Papas y un descuento del 30%. 
-- Por ej, podría pedir una big pdep del dia y debería ser como una big pdep (doble cuarto con curry)
--  pero con papas y el descuento del 30%. Por ejemplo una doble cuarto del dia deberia valer 88.
delDia :: Hamburguesa -> Hamburguesa
delDia = agregarIngrediente Papas . descuento 30

-- PARTE 3: algunos cambios más
-- Queremos modelar los siguientes modificadores:

-- - **hacerVeggie** :
--  cambia todos los ingredientes base que hayan en la hamburguesa por PatiVegano 
-- (ingrediente base tambien de precio 10) y el cheddar lo cambia por queso de almendras.
hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie = cambiarAPatiVegano . cambiarDeQueso 

-- -- -- CAMBIOS DE INGREDIENTE

-- -- NECESITO MEJORAR LA LOGICA REPETIDA
-- CONSIDERAR EL SIGUIENTE ejemplo
-- alterarLaVelocidad:: (Number -> Number ) -> Auto  -> Auto
-- alterarLaVelocidad    modificador auto = auto {velocidad = modificador.velocidad $auto}   

-- bajarVelocidad :: Number -> Auto -> Auto
-- bajarVelocidad cantidad = alterarLaVelocidad ( max 0 . subtract cantidad )



cambiarAPatiVegano :: Hamburguesa -> Hamburguesa
cambiarAPatiVegano hamburguesa =  hamburguesa {ingredientes =  sustituirIngrediente reemplazarPorPatyVegano ingredientesQueSonBase hamburguesa ++
                                                                         conservarIngredientes ingredientesQueNoSeanBase hamburguesa}

cambiarDeQueso :: Hamburguesa -> Hamburguesa
cambiarDeQueso hamburguesa = hamburguesa {ingredientes = sustituirIngrediente reemplazarPorQuesoDeAlmendras ingredientesQueSeanQuesoCheddar hamburguesa ++
                                                                        conservarIngredientes ingredienteQueNoSeanCheddar hamburguesa }

cambiarDePan :: Hamburguesa -> Hamburguesa
cambiarDePan hamburguesa = hamburguesa {ingredientes = sustituirIngrediente reemplazarPorPanIntegral ingredientesQueSeanPan hamburguesa ++
                                                                         conservarIngredientes ingredienteQueNoSeaPan hamburguesa }

-- - **cambiarPanDePati** : 
-- cambia el Pan que haya en la hamburguesa por PanIntegral (ingrediente de precio 3).
cambiarPanDePati :: Hamburguesa -> Hamburguesa
cambiarPanDePati = cambiarDePan 

-- FUNCIONES CLAVE

-- CONSERVAR

conservarIngredientes :: (Ingrediente -> Bool) -> Hamburguesa -> [Ingrediente]
conservarIngredientes criterio hamburguesa = filter criterio $ingredientes hamburguesa

-- SUSTITUIR

sustituirIngrediente :: (Ingrediente -> Ingrediente) -> (Ingrediente -> Bool) -> Hamburguesa -> [Ingrediente]
sustituirIngrediente ingredienteReemplazante criterioDeReemplazo hamburguesa = map ingredienteReemplazante . filter criterioDeReemplazo $ingredientes hamburguesa

-- CRITERIOS
ingredienteQueNoSeanCheddar :: Ingrediente -> Bool
ingredienteQueNoSeanCheddar ingrediente = ingrediente /= Cheddar
ingredienteQueNoSeaPan :: Ingrediente -> Bool
ingredienteQueNoSeaPan ingrediente = ingrediente /= Pan
ingredientesQueNoSeanBase :: Ingrediente -> Bool
ingredientesQueNoSeanBase ingrediente = ingrediente /= Pollo && ingrediente /= Carne
ingredientesQueSonBase :: Ingrediente -> Bool
ingredientesQueSonBase ingrediente = ingrediente == Pollo || ingrediente == Carne

ingredientesQueSeanPan :: Ingrediente -> Bool
ingredientesQueSeanPan ingredientes = ingredientes == Pan

ingredientesQueSeanQuesoCheddar :: Ingrediente -> Bool
ingredientesQueSeanQuesoCheddar ingredientes = ingredientes == Cheddar

-- REEMPLAZOS
reemplazarPorPatyVegano :: Ingrediente -> Ingrediente
reemplazarPorPatyVegano ingrediente = PatiVegano

reemplazarPorQuesoDeAlmendras :: Ingrediente -> Ingrediente
reemplazarPorQuesoDeAlmendras ingrediente = QuesoDeAlmendras

reemplazarPorPanIntegral :: Ingrediente -> Ingrediente
reemplazarPorPanIntegral ingredientes = PanIntegral

-- - hacer el **dobleCuartoVegano** que es un dobleCuarto veggie con pan integral.

dobleCuartoVegano :: Hamburguesa -> Hamburguesa
dobleCuartoVegano = cambiarAPatiVegano . cambiarDeQueso . cambiarPanDePati
