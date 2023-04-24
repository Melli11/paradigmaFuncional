 module Spec where
-- import PdePreludat
-- import Library
-- import Test.Hspec
-- import           Control.Monad (unless)

-- correrTests :: IO ()
-- correrTests = hspec $ do
--   suiteDeTestsDeParteI
--   suiteDeTestsDeParteII
--   --suiteDeTestsDeParteIV
  
-- suiteDeTestsDeParteI =
--   describe "ParteI: Tuplas" $ do

--     describe "first" $ do
--       it "first de una tupla de 3 elementos, me da el primero" $ do
--         first (1, 2, 3) `shouldBe` 1
--         first ("uno", 2, True) `shouldBe` "uno"
--         first (False, "dos", True) `shouldBe` False

--     describe "second" $ do
--       it "second de una tupla de 3 elementos, me da el segundo" $ do
--         second (1, 2, 3) `shouldBe` 2
--         second ("uno", 2, True) `shouldBe` 2
--         second (False, "dos", True) `shouldBe` "dos"

--     describe "third" $ do
--       it "third de una tupla de 3 elementos, me da el tercero" $ do
--         third (1, 2, 3) `shouldBe` 3
--         third ("uno", 2, True) `shouldBe` True
--         third (False, "dos", "tres") `shouldBe` "tres"

--     describe "swap" $ do
--       it "swap de una tupla con elementos iguales, me da la misma tupla" $ do
--         swap (0, 0) `shouldBe` (0, 0)
--       it "swap me transforma una tupla dando vuelta los elementos de lugar" $ do
--         swap (1, 2) `shouldBe` (2, 1)
--         swap ("hola", 3) `shouldBe` (3, "hola")

--     describe "divisionConResto" $ do
--       it "divisionConResto de un dividendo dividible por su divisor, me da el resultado de la division y 0" $ do
--         divisionConResto 4 2 `shouldBe` (2, 0)
--       it "divisionConResto de un dividendo no divisible por su divisor, me da el resultado de la division y el resto" $ do
--         divisionConResto 4 3 `shouldBe` (1, 1)

-- suiteDeTestsDeParteII =
--   describe "ParteII: Titulos" $ do

--     describe "edad" $ do
--       it "dado un estudiante, nos devuelve su edad" $ do
--         edad juan `shouldBe` 20 -- Por ej: si paso a Federico Romero que tiene 25 años y es Ingeniero, edad debería dar 25
--         edad juanFernandez `shouldBe` 26 -- Por ej: si paso a Federico Romero que tiene 25 años y es Ingeniero, edad debería dar 25
--         edad tincho `shouldBe` 30 -- Por ej: si paso a Federico Romero que tiene 25 años y es Ingeniero, edad debería dar 25
        
--     describe "nombreCompleto" $ do
--       it "si se pasa alguien sin título, devuelve el nombre y el apellido" $ do
--         nombreCompleto tincho `shouldBe` "Tincho Forever"-- Si paso a Ale Peralta, que tiene 23 años y no tiene titulo, nombre completo debería ser "Ale Peralta"
--       it "si se pasa alguien con ingenieria, devuelve el nombre y el apellido precedidos por Ing." $ do
--         nombreCompleto juanFernandez `shouldBe` "Ing. Juan Fernandez" -- Si paso a Federico Romero que tiene 25 años y es Ingeniero, deberia dar "Ing. Federico Romero"
--       it "si se pasa alguien con licenciatura, devuelve el nombre y el apellido precedidos por Lic." $ do
--         nombreCompleto juan `shouldBe` "Lic. Juan Topo"
--       it "si se pasa alguien con doctorado, devuelve el nombre y el apellido precedidos por Doc." $ do
--         nombreCompleto michael `shouldBe` "Doc. Rarito Jackson"

--     describe "recibirse" $ do
--       it "hacer que se reciba alguien con un doctorado nos devuelve a esa persona con 2 años más y con título de Doctorado" $ do
--         recibirse Doctorado tincho `shouldBe` tinchoConDoctorado -- Si paso a Ale Peralta, que tiene 23 años y no tiene titulo, debería recibir a Ale Peralta con 25 años y título de Doctorado
--       it "hacer que se reciba alguien con una licenciatura nos devuelve a esa persona con 4 años más y con título de Licenciatura" $ do
--         recibirse Licenciatura tincho `shouldBe` tinchoConLicenciatura
--       it "hacer que se reciba alguien con una ingenieria nos devuelve a esa persona con 6 años más y con título de Ingenieria" $ do
--         recibirse Ingenieria tincho `shouldBe` tinchoConIngenieria

-- {-Tincho original
-- tincho  = 
--   UnEstudiante
--     {
--     nombreEstudiante = "Tincho",
--     apellidoEstudiante = "Forever",
--     edadEstudiante = 30,
--     tituloEstudiante = Sin_Titulo 
--     }
-- -}

-- tinchoConDoctorado  = 
--   UnEstudiante
--     {
--     nombreEstudiante = "Tincho",
--     apellidoEstudiante = "Forever",
--     edadEstudiante = 32,
--     tituloEstudiante = Doctorado 
--     }

-- tinchoConLicenciatura = 
--   UnEstudiante
--     {
--     nombreEstudiante = "Tincho",
--     apellidoEstudiante = "Forever",
--     edadEstudiante = 34,
--     tituloEstudiante = Licenciatura 
--     }

-- tinchoConIngenieria = 
--   UnEstudiante
--     {
--     nombreEstudiante = "Tincho",
--     apellidoEstudiante = "Forever",
--     edadEstudiante = 36,
--     tituloEstudiante = Ingenieria 
--     }

--Ejemplo de Estudiante

-- juan  = 
--   UnEstudiante
--     {
--     nombreEstudiante = "Juan",
--     apellidoEstudiante = "Topo",
--     edadEstudiante = 20,
--     tituloEstudiante = TituloNormal Licenciatura   
--     }

-- tincho  = 
--   UnEstudiante
--     {
--     nombreEstudiante = "Tincho",
--     apellidoEstudiante = "Forever",
--     edadEstudiante = 30,
--     tituloEstudiante = TituloNormal Sin_Titulo  
--     }

-- michael =
--     UnEstudiante
--     {
--     nombreEstudiante = "Rarito",
--     apellidoEstudiante = "Jackson",
--     edadEstudiante = 99,
--     tituloEstudiante = TituloNormal Doctorado 
--     }

-- juanFernandez = 
--     UnEstudiante
--     {
--     nombreEstudiante = "Juan",
--     apellidoEstudiante = "Fernandez",
--     edadEstudiante = 26,
--     tituloEstudiante = TituloNormal Ingenieria 
--     }

-- alesito  = 
--     UnEstudiante
--     {
--     nombreEstudiante = "Ale",
--     apellidoEstudiante = "Cito",
--     edadEstudiante = 31,
--     tituloEstudiante = TituloDesarrollador Dev 5 
--     }
-- samoco  = 
--     UnEstudiante
--     {
--     nombreEstudiante = "Sam",
--     apellidoEstudiante = "Moco",
--     edadEstudiante = 34,
--     tituloEstudiante = TituloDesarrollador Dev 1 
--     }

-- leasco  = 
--     UnEstudiante
--     {
--     nombreEstudiante = "Lea",
--     apellidoEstudiante = "Asco",
--     edadEstudiante = 35,
--     tituloEstudiante = TituloDesarrollador Dev 3 
--     }


-- {-
-- suiteDeTestsDeParteIV =
--   describe "ParteIV: Devs" $ do

--     describe "recibirse" $ do
--       it "hacer que se recibia alguien como Dev con 0 años de experiencia nos devuelve a esa persona con 2 años mas y con titulo de Dev de 0 años de experiencia" $ do
--         escribime
--       it "hacer que se reciba alguien como Dev con ciertos años de experiencia nos devuelve a esa persona con esos años de experiencia + 2 y con título de Dev de esos años de experiencia" $ do
--         escribime

--     describe "nombreCompleto" $ do
--       it "si se pasa alguien con título de Dev con 0 años de experiencia, devuelve el nombre y el apellido precedidos por Dev Jr." $ do
--         escribime
--       it "si se pasa alguien con título de Dev con 1 año de experiencia, devuelve el nombre y el apellido precedidos por Dev Jr." $ do
--         escribime
--       it "si se pasa alguien con título de Dev con 2 años de experiencia, devuelve el nombre y el apellido precedidos por Dev Ssr." $ do
--         escribime
--       it "si se pasa alguien con título de Dev con entre 2 años y 5 años de experiencia, devuelve el nombre y el apellido precedidos por Dev Ssr." $ do
--         escribime
--       it "si se pasa alguien con título de Dev con 5 años de experiencia, devuelve el nombre y el apellido precedidos por Dev Ssr." $ do
--         escribime
--       it "si se pasa alguien con título de Dev con más de 5 años de experiencia, devuelve el nombre y el apellido precedidos por Dev Sr." $ do
--         escribime

--     describe "practicar" $ do
--       it "si hago practicar ciertos años a alguien con titulo de Dev de 0 años de experiencia, me devuelve una persona con esa cantidad de años de experiencia en su título y cuya edad aumento en la cantidad de años de practica" $ do
--         escribime
--       it "si hago practicar ciertos años a alguien con titulo de Dev con algunos años de experiencia, me devuelve una persona cuya experiencia en su título es lo que tenía más lo que le hice practicar y cuya edad aumento en la cantidad de años de practica" $ do
--         escribime
--       it "hacer practicar a una persona no Dev cierta cantidad de años me devuelve esa persona con esa cantidad de años más pero sin ningún otro cambio" $ do
--         escribime
-- -}

-- escribime :: Expectation
-- escribime = expectationFailure "Falta escribir el test"
