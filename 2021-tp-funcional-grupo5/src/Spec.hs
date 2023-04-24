module Spec where
import PdePreludat
import Library
import Test.Hspec

----CONSTRUCTORES DE PRUEBA

evangelina = Persona {    edad = 20, nombre = "Evangelina", listaDeSueños = [recibirseDe "arquitecta",recibirseDe "maestra",recibirseDe "veterinaria"], felicidonios = 12, listaDeHabilidades = ["Testear"] }
maximiliano = Persona {    edad = 30, nombre = "Maximiliano", listaDeSueños = [recibirseDe "medico",viajarA ["Madrid", "Tokio"]], felicidonios = 60, listaDeHabilidades = ["Nadar"] }
marcos = Persona {    edad = 28, nombre = "Marcos", listaDeSueños = [enamorarse maximiliano], felicidonios = 102, listaDeHabilidades = ["Nadar"] }
rarito = Persona {    edad = 28, nombre = "Rarito", listaDeSueños = [queTodoSigaIgual, enamorarse  raritoDos], felicidonios = 10, listaDeHabilidades = [] }
raritoDos = Persona {    edad = 28, nombre = "Rarito", listaDeSueños = [queTodoSigaIgual], felicidonios = 10, listaDeHabilidades = [] }
raritoTres = Persona {    edad = 28, nombre = "Rarito", listaDeSueños = [queTodoSigaIgual,enamorarse raritoTres], felicidonios = 10, listaDeHabilidades = [] }
-- PUNTO 1 A --
-- Constantes de prueba Coeficiente de Satisafacción --
personaMuyFeliz = Persona {   edad = 25, nombre = "Felix", listaDeSueños = [], felicidonios = 101, listaDeHabilidades = ["Nadar"] }
personaModeradamenteFelizconDosSueños = Persona {    edad = 26, nombre = "Matias", listaDeSueños = dosSueños, felicidonios = 100, listaDeHabilidades = [] }
personaPocoFeliz = Persona {    edad = 25, nombre = "Jose", listaDeSueños = [], felicidonios = 50, listaDeHabilidades = ["Nadar"]  }

-- PUNTO 1 B --
-- Constantes de prueba Grado de Ambición --
personaMuyFelizConDosSueños = Persona {    edad = 25, nombre = "Marcos", listaDeSueños = dosSueños, felicidonios = 101, listaDeHabilidades = ["Nadar"] }
personaPocoFelizConUnSueño = Persona {    edad = 25, nombre = "Jose", listaDeSueños = unSueño, felicidonios = 50, listaDeHabilidades = ["Nadar"] }

-- PUNTO 2 A --
-- Constantes de prueba tieneNombreLargo -- 
personaNombreLargo = Persona {    edad = 30, nombre = "Maximiliano", listaDeSueños = sinSueños, felicidonios = 28, listaDeHabilidades = ["Nadar"] }
personaNombreNormal = Persona {   edad = 20, nombre = "Evangelina", listaDeSueños = sinSueños, felicidonios = 28, listaDeHabilidades = ["Nadar"] }

-- PUNTO 2 B --
-- Constantes de prueba personaSuertuda -- 
personaConCoeficientePar = Persona {    edad = 25, nombre = "Soledad", felicidonios = 24, listaDeSueños = sinSueños, listaDeHabilidades = ["Nadar"] }
personaConCoeficienteImpar = Persona {    edad = 25, nombre = "Paulina", felicidonios = 28, listaDeSueños = sinSueños, listaDeHabilidades = ["Nadar"] }
-- PUNTO 2 C --
-- Constantes de prueba personaNombreComun -- 
personaNombreLindo = Persona {    edad = 25, nombre = "Melina", felicidonios = 24 ,listaDeSueños = sinSueños, listaDeHabilidades = ["Nadar"] }
personaNombreComun = Persona {    edad = 25, nombre = "Ariel", felicidonios = 28,listaDeSueños = sinSueños,listaDeHabilidades = ["Nadar"] }

-- PUNTO 3 --
-- Constantes de prueba test de Sueño -- 
unaPersona = personaModeradamenteFelizconDosSueños

-- Constantes auxiliares --
sinSueños = []
unSueño = [enamorarse personaPocoFeliz ]
dosSueños = [recibirseDe "abogado",viajarA["Paris","Roma","Glew"]]

correrTests :: IO ()
correrTests = hspec $ do

-- PUNTO 1 A --
  describe "Punto 1a - coeficienteDeSatisfaccion" $ do
    it "El coeficiente de satisfacción de una persona muy feliz  (101 felicidonios y 25 años) y sin sueños es  2525" $ do
      coeficienteDeSatisfaccion personaMuyFeliz  `shouldBe` 2525 
    it "El coeficiente de satisfacción de una persona moderadamente feliz (100 felicidonios y dos sueños) es  200 " $ do
      coeficienteDeSatisfaccion personaModeradamenteFelizconDosSueños `shouldBe` 200 
    it "El coeficiente de satisfacción de una persona poco feliz (50 felicidonios)  y sin sueños es  25 " $ do
      coeficienteDeSatisfaccion personaPocoFeliz  `shouldBe` 25

-- PUNTO 1 B --
  describe "Punto 1b - gradoDeAmbicion" $ do
    it "El grado de ambición de una persona muy feliz (101 felicidonios y con dos sueños) es  202"  $ do
      gradoDeAmbicion personaMuyFelizConDosSueños  `shouldBe` 202 
    it "El grado de ambición  de una persona moderadamente feliz (100 felicidonios, 26 años y dos sueños) es  52" $ do
      gradoDeAmbicion personaModeradamenteFelizconDosSueños  `shouldBe` 52 
    it "El grado de ambición  de una persona poco feliz (50 felicidonios y un sueño) es  2" $ do
      gradoDeAmbicion personaPocoFelizConUnSueño  `shouldBe` 2

-- PUNTO 2 A --  
  describe "Punto 2a - tieneNombreLargo" $ do
    it "Una persona tiene un nombre normal si su nombre tiene menos de 10 letras" $ do
      tieneNombreLargo  personaNombreNormal `shouldBe` False
    it "Una persona tiene un nombre largo si su nombre tiene mas de 10 letras" $ do
      tieneNombreLargo personaNombreLargo  `shouldBe` True

-- PUNTO 2 B --     
  describe "Punto 2b - personaSuertuda" $ do
    it "Una persona cuyo coeficiente de satisfaccion es impar es una persona sin suerte " $ do
      personaSuertuda personaConCoeficienteImpar `shouldBe` False
    it "Una persona cuyo coeficiente de satisfaccion es par es una persona con suerte" $ do
      personaSuertuda personaConCoeficientePar  `shouldBe` True

-- PUNTO 2 C --     
  describe "Punto 2c - personaNombreComun" $ do
    it "Una persona  tiene un nombre comun si la ultima letra de su nombre NO finaliza en 'a' " $ do
      tieneNombreLindo personaNombreComun `shouldBe` False
    it "Una persona  tiene un nombre lindo si la ultima letra de su nombre finaliza en 'a' " $ do
      tieneNombreLindo personaNombreLindo `shouldBe` True 

-- PUNTO 3 A --
  describe "Punto 3a - Test Sueños de una persona" $ do
    it "Dado que una persona se recibe de una carrera, entonces aumentan sus felicidonios"  $ do 
          (felicidonios.recibirseDe "medicina") unaPersona `shouldBe` 220
    it "Dado que una persona se recibe de una carrera, entonces gana una habilidad  " $ do      
         (length . listaDeHabilidades . recibirseDe "medicina") unaPersona `shouldBe` 1

-- PUNTO 3 B --
  describe "Punto 3b - Test Sueños de una persona" $ do
    it "Dado que una persona realizó un viaje o más sumará 100 felicidonios por cada ciudad visitada " $ do      
         (felicidonios. viajarA ["Lanus","Solano"]) unaPersona `shouldBe` 300 
    it "Dado que una persona realizó un viaje o más entonces al finalizarlo tendrá un año mas  " $ do      
         (edad . viajarA ["Lanus","Solano"]) unaPersona `shouldBe` 27

-- PUNTO 3 C --
  describe "Punto 3c - Test Sueños de una persona" $ do
    it "Si una persona  X  se enamora de otra persona Y , la persona X ganará los felicidonios de la persona Y " $ do      
         (felicidonios. enamorarse unaPersona )  personaPocoFeliz `shouldBe` 150 
    it "El sueño no es bidireccional, que X se enamore de Y no implica lo que Y se enamore de X " $ do
        felicidonios unaPersona  `shouldBe` 100
        felicidonios personaPocoFeliz  `shouldBe` 50
        (felicidonios. enamorarse unaPersona )  personaPocoFeliz `shouldBe` 150

-- PUNTO 3 D --
  describe "Punto 3d - Test Sueños de una persona" $ do
    it "Si una persona tiene un sueño conformista la persona permanecerá en el mismo estado " $ do      
         (nombre.queTodoSigaIgual) unaPersona `shouldBe` "Matias" 
         (edad.queTodoSigaIgual) unaPersona `shouldBe`  26
         (felicidonios.queTodoSigaIgual) unaPersona `shouldBe`  100
         (listaDeHabilidades.queTodoSigaIgual) unaPersona `shouldBe` []
            
-- PUNTO 3 E --
  describe "Punto 3e - Test Sueños de una persona" $ do
    it "Si una persona tiene un combo perfecto se recibirá de Medicina y aumentará sus felicidonios " $ do
          (listaDeHabilidades.comboPerfecto) unaPersona `shouldBe` ["Medicina"]
    it "Si una persona tiene un combo perfecto viajará a Watcher's Hills y Paris y aumentará su edad en 1 año" $ do  
          (edad.comboPerfecto) unaPersona `shouldBe` 27    
    it "Si una persona tiene un combo perfecto luego de viajar a Watcher's Hills y Paris y recibirse de Medicina,  aumentará sus felicidonios en 320 unidades" $ do  
          (felicidonios.recibirseDe "Medicina".viajarA["Watcher's Hills","Paris"]) unaPersona `shouldBe` 420          
    it "Si una persona tiene un combo perfecto sumará 100 felicidonios extras " $ do      
          (felicidonios.bonusExtra.recibirseDe "Medicina".viajarA["Watcher's Hills","Paris"]) unaPersona `shouldBe` 520


-- PUNTO 4 A --
  describe "Punto 4a - Fuente minimalista" $ do
    it "Si una persona pide sus deseos en una fuente minamilista entonces se cumplirá su primer deseo y aumentarán sus felicidonios" $ do
          (felicidonios.fuenteMinimalista) unaPersona `shouldBe` 205
    it "Si una persona pide sus deseos en una fuente minamilista luego de cumplir su 1er deseo el mismo se eliminará de su lista de deseos" $ do  
          (length.listaDeSueños.fuenteMinimalista) unaPersona `shouldBe` 1    
    it "Si una persona pide sus deseos en una fuente minamilista luego de cumplir su 1er deseo el mismo se agregará como habilidad" $ do  
          (listaDeHabilidades.fuenteMinimalista) unaPersona `shouldBe` ["abogado"]    
        --  dosSueños = [recibirseDe "abogado",viajarA["Paris","Roma","Glew"]]
    
{-
-- PUNTO 4 B --
  describe "Punto 4b - Fuente copada" $ do
    it "Si una persona pide sus deseos en una fuente copada entonces se cumplirán todos sus deseos y aumentarán sus felicidonios" $ do
          (felicidonios.fuenteCopada) unaPersona `shouldBe` 505
    it "Si una persona pide sus deseos en una fuente copada luego de cumplir todos sus deseos los mismos se eliminarán de su lista de deseos" $ do  
          (length.listaDeSueños.fuenteCopada) unaPersona `shouldBe` 0    
    it "Si una persona pide sus deseos en una fuente copada luego de cumplir  todos sus deseos los mismos se agregarán como habilidad" $ do  
          (listaDeHabilidades.fuenteCopada) unaPersona `shouldBe` ["abogado"]    
  -}
-- PUNTO 4 C --
  describe "Punto 4c - Fuente a pedido" $ do
    it "Si una persona pide un determinado deseo en una fuente a pedido entonces se cumplirá ese deseo y aumentarán sus felicidonios" $ do
          (felicidonios.fuenteApedido 1) unaPersona `shouldBe` 205
          (felicidonios.fuenteApedido 2) unaPersona `shouldBe` 400
    it "Si una persona pide sus deseos en una fuente a pedido luego de cumplir ese  deseo el mismo NO se eliminará de su lista de deseos" $ do  
          (length.listaDeSueños.fuenteApedido 1) unaPersona `shouldBe` 2    
          (length.listaDeSueños.fuenteApedido 2) unaPersona `shouldBe` 2    
    it "Si una persona pide sus deseos en una fuente a pedido luego de cumplir  ese  deseo el mismo se agregará como habilidad" $ do  
          (listaDeHabilidades.fuenteApedido 1) unaPersona `shouldBe` ["abogado"]    
          (listaDeHabilidades.fuenteApedido 2) unaPersona `shouldBe` []    
    
-- PUNTO 4 D --
  describe "Punto 4d - Fuente a pedido" $ do
    it "Si una persona pide sus deseos en una fuente sorda  entonces no cumplirá ningún deseo y sus felicidonios no aumentarán" $ do
          (felicidonios.fuenteSorda) unaPersona `shouldBe` 100
    it "Si una persona pide sus deseos en una fuente sorda  entonces ese  deseo NO se eliminará de su lista de deseos" $ do  
          (length.listaDeSueños.fuenteSorda) unaPersona `shouldBe` 2    
    it "Si una persona pide sus deseos en una fuente sorda  entonces ese  deseo NO  se agregará como habilidad" $ do  
          (listaDeHabilidades.fuenteSorda) unaPersona `shouldBe` []

-- PUNTO 6 A --
  describe "Punto 6a - sueñosValiosos" $ do
    it "Si una persona que posee sueños valiosos (2) entonces al cumplir c/u de ellos la persona quedará con más de 100  felicidonios " $ do
           map (felicidoniosSueñoCumplido unaPersona ) (sueñosValiosos unaPersona) `shouldBe` [205,400] 
    it "Si una persona NO posee sueños valiosos entonces al cumplir c/u de ellos la persona quedará con menos de 100  felicidonios " $ do
           map (felicidoniosSueñoCumplido rarito ) (sueñosValiosos rarito) `shouldBe` []       
    it "Si una persona posee 2 sueños valiosos entonces su cantidad de sueños valiosos serán 2" $ do  
          (length.sueñosValiosos) unaPersona `shouldBe` 2    
    it "Si una persona NO posee sueños valiosos entonces su cantidad de sueños valiosos serán 0" $ do  
          (length.sueñosValiosos) rarito `shouldBe` 0
    
-- PUNTO 6 B --
  describe "Punto 6b - sueñosRaros" $ do
    it "Si una persona posee sueños raros entonces al cumplir ALGUNO de ellos la persona quedará con la misma cantidad de felicidonios " $ do
           tieneAlgunSueñoRaro rarito `shouldBe` True 
    it "Si una persona NO posee sueños raros entonces al cumplir c/u de ellos la persona quedará con mas felicidonios" $ do
           tieneAlgunSueñoRaro unaPersona `shouldBe` False        

{-
-- PUNTO 6 C --
  describe "Punto 6c - felicidadDelGrupo" $ do
    it "Dado el grupo de personas Evangelina,Maximiliano y Marcos al cumplirse todos sus sueños sumarán una cantidad total de 1144 de felicidad " $ do
           felicidadDelGrupo [evangelina ,maximiliano ,marcos] `shouldBe` 1144  
    it "Dado el grupo de personas conformado solo por Rarito al cumplirse todos sus sueños sumará una cantidad total de 20 de felicidad " $ do
           felicidadDelGrupo [rarito] `shouldBe` 20  
    it "Dado el grupo de personas sin personas al cumplirse todos sus sueños sumarán una cantidad total de 0 de felicidad " $ do
           felicidadDelGrupo [] `shouldBe` 0
-}