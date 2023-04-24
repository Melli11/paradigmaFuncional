module Library where
import PdePreludat


type Habilidades = String

type Deseo = Chico -> Chico

data Chico = Chico {
    nombre :: String,
    edad :: Number,
    habilidad :: [Habilidades],
    deseo :: [Deseo]
    } deriving (Show,Eq)

chico :: Chico
chico = Chico "bobi" 10 ["Ser un supermodelo noruego"] [serMayor]

chico2 :: Chico
chico2 = Chico "bobi2" 10 ["Ser un supermodelo noruego"] [serMayor]

chicoConHabilidad :: Chico
chicoConHabilidad  = Chico "bobiHabilidoso" 10 ["Prog Haskell Super Sr"] []

chicoPeligroso :: Chico
chicoPeligroso = Chico "bobiPeligroso" 10 ["Conducir"] [serMayor]

timmy :: Chico
timmy = Chico "Timmy" 10 ["mirar television", "jugar en la pc"] [serMayor]

taxiDriver :: Chico
taxiDriver  = Chico "bobiPeligroso" 19 ["Sabe manejar"] []

chicoSinHabilidadCon2Deseos :: Chico
chicoSinHabilidadCon2Deseos = Chico "bobiDosDeseos" 10 [] [aprenderHabilidades ["Andar en bicicleta"],serMayor]
-- 1. **Concediendo deseos**
aprenderHabilidades :: [Habilidades] -> Deseo
aprenderHabilidades nuevasHabilidades unChico = unChico {habilidad = nuevasHabilidades ++ habilidad unChico}

serMayor :: Deseo
serMayor unChico = unChico {edad = 18}
-- serGrosoEnNeedForSpeed unChico = aprenderHabilidades 

--  1. wanda: dado un chico, wanda le cumple el primer deseo 
-- y lo hace madurar (crecer un año de edad).

wanda :: Deseo
wanda = controlarEdad (+) 1 . cumplirDeseo Uno

data NumeroDESEO = Uno | Ninguno | Todos
cumplirDeseo :: NumeroDESEO -> Deseo
cumplirDeseo Uno unChico = (head $deseo unChico) unChico
cumplirDeseo Ninguno unChico =  unChico
cumplirDeseo Todos unChico =  foldl aplicarUnDeseo unChico $deseo unChico

aplicarUnDeseo :: Chico -> Deseo -> Chico
aplicarUnDeseo unChico unDeseo = unDeseo unChico

controlarEdad :: (Number-> Number-> Number) -> Number ->  Chico -> Chico
controlarEdad operacion numero unChico =  unChico {edad = operacion numero (edad unChico)}

--   2. cosmo: dado un chico, lo hace “des”madurar, quedando con la mitad de años de edad. 
-- Como es olvidadizo, no le concede ningún deseo.

cosmo :: Deseo
cosmo = controlarEdad (*) 0.5 . cumplirDeseo Ninguno

--   3. muffinMagico: dado un chico le concede todos sus deseos. 

muffinMagico :: Deseo
muffinMagico = cumplirDeseo Todos

-- 2. **En busqueda de pareja** 
-- 1. tieneHabilidad unaHabilidad unChico : Dado un chico y una habilidad, dice si la posee.

tieneHabilidad :: Habilidades -> Chico -> Bool
--tieneHabilidad unaHabilidad unChico = elem unaHabilidad $habilidad unChico
tieneHabilidad unaHabilidad  = elem unaHabilidad . habilidad

esSuperMaduro :: Chico -> Bool
esSuperMaduro unChico = tieneHabilidad "Sabe manejar" unChico && ((>18).edad) unChico

-- para Trixie la única condición es que el chico no sea Timmy, 

data Chica = Chica {
    nombreChica :: String,
    condicionDeEleccion :: Chico -> Bool
    } deriving (Show,Eq)

trixie :: Chica
trixie = Chica "Trixie Tang" noEsTimmy
vicky :: Chica
vicky = Chica "Vicky" (tieneHabilidad "Ser un supermodelo noruego")

noEsTimmy :: Chico -> Bool
noEsTimmy =  (/= "Timmy").nombre
noEsTimmy' unChico = nombre unChico /= "Timmy"

pretendientes :: [Chico]
pretendientes = [chico,taxiDriver,chicoSinHabilidadCon2Deseos,chico2]

pretendientesFinal :: [Chico]
pretendientesFinal = [taxiDriver,chicoSinHabilidadCon2Deseos]

quienConquistaA :: Chica -> [Chico] -> Chico
quienConquistaA unaChica listaDePretendientes
        | any ((True ==) . condicionDeEleccion unaChica) listaDePretendientes = head $filter (condicionDeEleccion unaChica) listaDePretendientes
        | otherwise = last listaDePretendientes


-- Dar un ejemplo de consulta para una nueva chica, cuya condición para elegir a un chico es que este sepa cocinar
nuevaChica :: Chica
nuevaChica = Chica "Gula" (tieneHabilidad "Que sepa cocinar")

-- -   **infractoresDeDaRules**:  Dada  una  lista  de  chicos,  devuelve  la  lista  de  los  nombres  de  aquellos que tienen deseos prohibidos.
-- Un deseo  está  prohibido  si,  al  aplicarlo, -- entre  las  cinco primeras habilidades, hay alguna prohibida.
--   En  tanto,  son  habilidades  prohibidas  enamorar,  matar y dominar el mundo. 

-- -- ejemplo de chico con deseo prohibido
-- galan :: Chico
-- galan = Chico "Tarzan" 25 [] [aprenderHabilidades ["enamorar"]]
-- -- ejemplo de chico con deseo prohibido en la 6ta posicion deberia no ser infractor
-- asesinofallido :: Chico
-- asesinofallido = Chico "Chapa de plastico" 20 [] [serMayor,serMayor,serMayor,serMayor,serMayor,aprenderHabilidades ["asesino"]]
-- asesino :: Chico
-- asesino = Chico "Ted Bundy" 30 [] [aprenderHabilidades ["dominar el mundo"],aprenderHabilidades ["secuestrar"],aprenderHabilidades ["enganar"],aprenderHabilidades ["sociabilizar"],aprenderHabilidades ["matar"]]

asesinofallido = Chico "Chapa de plastico" 20 [] [aprenderHabilidades ["matar"],aprenderHabilidades ["nadar"],aprenderHabilidades ["cantar"],aprenderHabilidades ["saltar"],aprenderHabilidades ["remar"],aprenderHabilidades ["imitar"]]



deseoProhibido :: String -> Bool
deseoProhibido deseo =  deseo == "enamorar" || deseo == "matar" || deseo == "dominar el mundo"

algunoConDeseosProhibidos :: Chico -> Bool
algunoConDeseosProhibidos  unChico = any deseoProhibido  $(take 5.habilidad.cumplirDeseo Todos) unChico

infractoresDeDaRules :: [Chico] -> [String]
infractoresDeDaRules = map nombreDeUnChico . filter algunoConDeseosProhibidos

nombreDeUnChico :: Chico -> String
nombreDeUnChico = nombre



-- ya implementadas
-- cumplirDeseo Todos unChico =  foldl aplicarUnDeseo unChico $deseo unChico  
-- tieneHabilidad unaHabilidad  = elem unaHabilidad . habilidad

-- infractoresDeDaRules :: [Chico] -> [String]
