module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero
type Cansancio = Number
type Stress = Number
-- type Energia = (Cansancio,Stress)

data Turista = Turista {
    cansancio :: Number,
    stress :: Number,
    viajaSolo :: Bool,
    idiomasQueHabla :: [String]
}deriving(Show,Eq)

-- FUNCIONES AUXILIARES
modificarStressEn :: Number -> Turista -> Turista
-- modificarStressEn cantidad (Turista (cansancio,stress) compania idiomas ) = Turista (cansancio,stress + cantidad) compania idiomas --Para la version con tupla energia
modificarStressEn cantidad (Turista cansancio stress compania idiomas ) = Turista cansancio (stress + cantidad) compania idiomas
modificarCansancioEn :: Number -> Turista -> Turista
-- modificarCansancioEn cantidad (Turista (cansancio,stress) compania idiomas ) = Turista (cansancio + cantidad,stress) compania idiomas --Para la version con tupla energia
modificarCansancioEn cantidad (Turista cansancio stress compania idiomas ) = Turista (cansancio + cantidad) stress  compania idiomas

elTurisTaViajaAcompañado turista = not (viajaSolo turista)
-- modificarCansancioEn cantidad 

meloso :: Turista
meloso = Turista 4 10 False ["Castellano"]

sasuke = Turista 100 50 True ["Japones","Español"]


-- irAlaPlaya :: Turista -> Turista
-- irAlaPlaya unTurista
--     | viajaSolo unTurista = unTurista {energia =  bajarCansancioEn 5 (energia unTurista)}
--     | otherwise = unTurista {energia = bajarStressEn 1 (energia unTurista)}

irAlaPlaya :: Turista -> Turista
irAlaPlaya unTurista
    | viajaSolo unTurista = modificarCansancioEn (-5) unTurista
    | otherwise = modificarStressEn (-1) unTurista

apreciarAlgunElementoDelPaisaje :: String -> Turista -> Turista
-- apreciarAlgunElementoDelPaisaje elemento unTurista = unTurista {energia = bajarStressEn (length elemento) (energia unTurista)}

-- apreciarAlgunElementoDelPaisaje  = modificarStressEn.((-1)*length)
apreciarAlgunElementoDelPaisaje elemento = modificarStressEn ( (-1)* length elemento)

salirAHablarUnIdioma :: String -> Turista -> Turista
salirAHablarUnIdioma unIdioma (Turista cansancio stress _ idiomasQueSabe) = Turista cansancio stress False (unIdioma:idiomasQueSabe)

-- caminar :: Number -> Turista -> Turista
-- caminar cantidadDeMinutos unTurista = unTurista {energia = afectarSegunTiempo cantidadDeMinutos (energia unTurista) }

caminar :: Number -> Turista -> Turista
caminar cantidadDeMinutos  = modificarCansancioEn (div cantidadDeMinutos 4) . modificarStressEn (div (-cantidadDeMinutos) 4)

-- afectarSegunTiempo :: Number -> (Number, Number) -> (Number, Number)
-- afectarSegunTiempo tiempo (cansancio,stress) =  (cansancio + tiempo/4,stress - tiempo/4)

data NiveldeLaMarea = Fuerte | Moderada | Tranquila

-- paseoEnBarco ::  NiveldeLaMarea -> Turista -> Turista
paseoEnBarco :: NiveldeLaMarea -> Turista -> Turista
paseoEnBarco Fuerte  = modificarStressEn 6 . modificarCansancioEn 10
paseoEnBarco Tranquila  = caminar 10 . apreciarAlgunElementoDelPaisaje "mar" . salirAHablarUnIdioma "aleman"
paseoEnBarco Moderada  = id


-- Crear un modelo para los turistas y crear los siguientes tres ejemplos:
-- Ana: está acompañada, sin cansancio, tiene 21 de stress y habla español.
ana = Turista 0 21 False ["Español"]

-- Beto y Cathi, que hablan alemán, viajan solos, y Cathi además habla catalán. Ambos tienen 15
-- unidades de cansancio y stress.
beto = Turista 15 15 True ["Aleman"]
cathi = Turista 15 15 True ["Aleman","Catalan"]

-- 2. Modelar las excursiones anteriores de forma tal que para agregar una excursión al sistema no haga
-- falta modificar las funciones existentes. Además:

type Excursion = Turista -> Turista

-- a. Hacer que un turista haga una excursión. Al hacer una excursión, el turista además de sufrir los
-- efectos propios de la excursión, reduce en un 10% su stress.


stressDelTuristaEn10Menos :: Turista -> Turista
stressDelTuristaEn10Menos  (Turista cansancio stress compania idiomas ) =  Turista cansancio (stress*0.90) compania idiomas

hacerUnaExcursion :: Turista -> Excursion -> Turista
hacerUnaExcursion  unTurista =  stressDelTuristaEn10Menos .  aplicarExcursion unTurista

aplicarExcursion unTurista excursion = excursion unTurista



-- b. Dada la función
deltaSegun :: (a -> Number) -> a -> a -> Number
deltaSegun f algo1 algo2 = f algo1 - f algo2

-- Definir la función deltaExcursionSegun que a partir de un índice, un turista y una excursión
-- determine cuánto varió dicho índice después de que el turista haya hecho la excursión.
-- Llamamos índice a cualquier función que devuelva un número a partir de un turista

deltaExcursionSegun :: (Turista -> Number) -> Turista -> Excursion -> Number
deltaExcursionSegun indice unTurista excursion = deltaSegun indice (hacerUnaExcursion unTurista excursion) unTurista

-- Usar la función anterior para resolver cada uno de estos puntos:
-- i. Saber si una excursión es educativa para un turista, que implica que termina
-- aprendiendo algún idioma.

esUnaExcursionEsEducativa ::  Turista -> Excursion  -> Bool
esUnaExcursionEsEducativa unTurista = (0<) . deltaExcursionSegun (length.idiomasQueHabla) unTurista

laExcursionEsEducativa' :: Excursion -> Turista  -> Bool
laExcursionEsEducativa' excursion unTurista
    | cambiosEnElVocabularioDelTurista unTurista excursion  == 0 = False
    | otherwise = True

cambiosEnElVocabularioDelTurista :: Turista -> Excursion -> Number
cambiosEnElVocabularioDelTurista = deltaExcursionSegun (length.idiomasQueHabla)

-- ii. Conocer las excursiones desestresantes para un turista. Estas son aquellas que le
-- reducen al menos 3 unidades de stress al turista.

esUnaExcursionDesestresante :: Turista -> Excursion -> Bool
esUnaExcursionDesestresante unTurista  = (<= -3 ) . deltaExcursionSegun stress unTurista


-- Para mantener a los turistas ocupados todo el día, la empresa vende paquetes de excursiones
-- llamados tours. Un tour se compone por una serie de excursiones.

-- - Completo: Comienza con una caminata de 20 minutos para apreciar una "cascada", luego se
-- camina 40 minutos hasta una playa, y finaliza con una salida con gente local que habla
-- "melmacquiano".

tourCompleto :: [Excursion]
tourCompleto = [caminar 20, apreciarAlgunElementoDelPaisaje "cascada", caminar 40, salirAHablarUnIdioma "melmaquiano"]

tourLadoB = [paseoEnBarco Tranquila, irAlaPlaya, caminar 120]

islaVecina = [paseoEnBarco Tranquila, irAlaPlaya, paseoEnBarco Tranquila]

realizarTour :: Turista -> [Excursion] -> Turista
realizarTour  unTurista tour  = modificarStressEn (length tour) $ foldl hacerUnaExcursion unTurista tour

-- b. Dado un conjunto de tours, saber si existe alguno que sea convincente para un turista. Esto
-- significa que el tour tiene alguna excursión desestresante la cual, además, deja al turista
-- acompañado luego de realizarla.

esUnTourConvincente :: Turista -> [Excursion] -> Bool
esUnTourConvincente unTurista tour = elTurisTaViajaAcompañado $ (head $ filter (esUnaExcursionDesestresante unTurista) tour) unTurista

-- laExcursionEsDesestresante :: Turista -> Excursion -> Bool
-- laExcursionEsDesestresante unTurista  = (<= -3 ) . deltaExcursionSegun stress unTurista


-- c. Saber la efectividad de un tour para un conjunto de turistas. Esto se calcula como la sumatoria
-- de la espiritualidad recibida de cada turista a quienes les resultó convincente el tour.
-- La espiritualidad que recibe un turista es la suma de las pérdidas de stress y cansancio tras el
-- tour.

-- efectividadDeUnTour grupoDeTuristas tour =  filter (flip esUnTourConvincente tour) 

-- espiritualidad = 

naturales = take 15 $ (1+):naturales

