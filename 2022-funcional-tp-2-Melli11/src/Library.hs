module Library where
import PdePreludat

-- 1. Tuplas

first ::  (a, b, c) -> a
first (primer_elemento,segundo_elemento,tercer_elemento) = primer_elemento

second :: (a, b, c) -> b
second  (primer_elemento,segundo_elemento,tercer_elemento) = segundo_elemento

third :: (a, b, c) -> c
third (primer_elemento,segundo_elemento,tercer_elemento) = tercer_elemento

swap :: (b, a) -> (a, b)
swap (primer_elemento,segundo_elemento) = (segundo_elemento, primer_elemento) 

divisionConResto :: Number -> Number -> (Number, Number)
divisionConResto dividendo divisor = (div dividendo divisor,rem dividendo divisor)


-- 2. Titulos

type AniosDeExperiencia = Number
data Titulo  =  Ingenieria  | Licenciatura | Doctorado | Dev AniosDeExperiencia |  Sin_Titulo  deriving(Show,Eq)
      
data Estudiante = UnEstudiante
    {
    nombreEstudiante :: String ,
    apellidoEstudiante :: String,
    edadEstudiante :: Number,
    tituloEstudiante :: Titulo 
    }
    deriving(Show,Eq)

tituloAbreviado  :: Titulo -> Estudiante -> String

tituloAbreviado (TituloNormal Ingenieria) estudiante = "Ing."
tituloAbreviado (TituloNormal Licenciatura) estudiante = "Lic."
tituloAbreviado (TituloNormal Doctorado) estudiante = "Doc."
tituloAbreviado (TituloNormal Sin_Titulo) estudiante = "Sin titulo"

tituloAbreviado (TituloDesarrollador Dev aniosDeExperiencia) estudiante 
    | esDesarrolladorJunior aniosDeExperiencia estudiante = "Dev Jr."
    | esDesarrolladorSSr aniosDeExperiencia estudiante = "Dev SSr."
    | otherwise = "Dev Sr."

esDesarrolladorJunior :: Number -> Estudiante -> Bool
esDesarrolladorJunior seniority estudiante = seniority <= 1  

esDesarrolladorSSr :: Number -> Estudiante -> Bool
esDesarrolladorSSr seniority estudiante = seniority > 1 && seniority < 5  

edad :: Estudiante -> Number
edad (UnEstudiante _ _ edadEstudiante _ ) = edadEstudiante

nombreCompleto :: Estudiante -> String
nombreCompleto estudiante
    | noTieneTitulo estudiante = retornoNombreCompleto estudiante
    | otherwise =  retornoTituloAbreviadoyNombreCompleto estudiante


noTieneTitulo :: Estudiante -> Bool
noTieneTitulo (UnEstudiante _ _ _ (TituloNormal Sin_Titulo)) = True   
noTieneTitulo (UnEstudiante _ _ _ (TituloNormal _)) = False
noTieneTitulo (UnEstudiante _ _ _ (_)) = False

retornoNombreCompleto :: Estudiante -> String
retornoNombreCompleto estudiante = nombreEstudiante estudiante ++ " " ++ apellidoEstudiante estudiante 

retornoTituloAbreviadoyNombreCompleto :: Estudiante -> String
retornoTituloAbreviadoyNombreCompleto estudiante = tituloAbreviado (tituloEstudiante estudiante) estudiante ++ " " ++ retornoNombreCompleto estudiante

recibirse :: Titulo -> Estudiante -> Estudiante
recibirse titulo persona = persona {edadEstudiante = edadEstudiante persona + tiempoEnRecibirse titulo persona, tituloEstudiante = titulo}

tiempoEnRecibirse :: Titulo -> Estudiante -> Number
tiempoEnRecibirse (TituloNormal Ingenieria) estudiante = 6
tiempoEnRecibirse (TituloNormal Licenciatura) estudiante = 4
tiempoEnRecibirse (TituloNormal Doctorado)  estudiante = 2
tiempoEnRecibirse (TituloDesarrollador Dev aniosDeExperiencia) estudiante 
    | aniosDeExperiencia == 0 = 2
    | otherwise = aniosDeExperiencia + 2


-- 4. Devs
-- Finalmente, se puede ganar experiencia luego de haberse recibido de Dev, así que queremos plasmar esto con la siguiente función:

-- practicar: que recibe una persona y una cantidad de años de práctica y nos devuelve a la persona envejecida en esa cantidad de años y aparte:
-- si era Dev, aumenta sus años de experiencia en esos años de práctica.
-- si su título era cualquier otro o no tenía, no cambia nada más

{-practicar :: Estudiante -> Number -> Estudiante
practicar persona aniosDePractica  
    | esDesarrollador persona = envejecer (incrementarExperiencia persona aniosDePractica)
    | otherwise = envejecer persona

esDesarrollador :: Estudiante -> Bool
esDesarrollador persona = (tituloEstudiante persona) == TituloDesarrollador

-}

--incrementarExperiencia :: Estudiante -> Number -> Estudiante


--incrementarExperiencia  (UnEstudiante _ _ _ (TituloDesarrollador _ experiencia)) aniosDeExperiencia = experiencia + aniosDeExperiencia 

--incrementarExperiencia  estudiante aniosDeExperiencia = estudiante { tituloEstudiante = { experiencia = experiencia (tituloEstudiante estudiante) + aniosDeExperiencia  }}  

-- incrementarExperiencia :: Estudiante -> Number -> Estudiante
-- incrementarExperiencia estudiante aniosDePractica  = estudiante {tituloEstudiante = TituloDesarrollador 
-- { tituloDev = tituloDev (tituloEstudiante estudiante), experiencia = experiencia (tituloEstudiante estudiante) + aniosDePractica }}


-- incrementarExperiencia  estudiante aniosDeExperiencia = UnEstudiante { 
--     nombreEstudiante = nombreEstudiante estudiante ,
--     apellidoEstudiante = apellidoEstudiante estudiante,
--     edadEstudiante = edadEstudiante estudiante,
--     tituloEstudiante = TituloDesarrollador { 
--     tituloDev = tituloDev (tituloEstudiante estudiante),
--     experiencia = experiencia (tituloEstudiante estudiante) + aniosDeExperiencia }
--     }


--envejecer :: Estudiante -> Number -> Estudiante
--envejecer persona aniosDePractica = persona {edadEstudiante = edadEstudiante persona + aniosDePractica}

-- juan  = 
--   UnEstudiante
--     {
--     nombreEstudiante = "Juan",
--     apellidoEstudiante = "Topo",
--     edadEstudiante = 20,
--     tituloEstudiante = TituloNormal Licenciatura   
--     }

alesito  = 
    UnEstudiante
    {
    nombreEstudiante = "Ale",
    apellidoEstudiante = "Cito",
    edadEstudiante = 31,
    tituloEstudiante = TituloDesarrollador Dev 5 
    }
    
-- data Titulo =
--       TituloNormal { titulo :: Titulos } |
--       TituloDesarrollador {tituloDev :: TituloDev, experiencia ::AniosDeExperiencia}
--       deriving (Show,Eq)

-- data Estudiante = 
--   UnEstudiante
--     {
--     nombreEstudiante :: String ,
--     apellidoEstudiante :: String,
--     edadEstudiante :: Number,
--     tituloEstudiante :: Titulo 
--     }
--     deriving(Show,Eq)

{-  

-- 3. Pregunta

--Si yo tengo a un estudiante que tiene 26 años, se llama Juan Fernandes y tiene título de Ingeniería:

juan = implementame -- Escriban el código de como sería

-- y en ghci evaluo el código que haría que juan se reciba de un doctorado

-- Y luego le pido a ghci que evalue

-- edad juan

-- ¿cuantos años me va a devolver esa ultima consulta? ¿Por qué?

-- Respuesta: 

En ghci: 

1) Evalúo a Juan Fernandez

Spec Library Paths_tp Spec> recibirse Ingenieria juanFernandez  
UnEstudiante {nombreEstudiante = "Juan", apellidoEstudiante = "Fernandez", edadEstudiante = 32, tituloEstudiante = Ingenieria}

2) edad juanFernandez 

26

RTA: Como se puede ver en los puntos 1) y 2) demostrados en ghci, la edad de Juan Fernandez
se resuelve con diferentes valores, 32 y 26 respectivamente, esto se debe a que el paradigma funcional resuelve
ambas ecuaciones respetando la definición matemática de la variable y las considera como consultas independientes.
Se puede notar la  diferencia en contraste al  paradigma imperativo donde las variables representan posiciones de 
memoria en las que se almacenan valores.  
-}