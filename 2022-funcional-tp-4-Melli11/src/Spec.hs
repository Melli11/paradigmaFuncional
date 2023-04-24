module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
  suiteDeTestsDePartes1y2
  suiteDeTestsDeParte3
  --suiteDeTestsDeParte4 -- descomenta esto solo si vas a hacer el punto bonus

suiteDeTestsDePartes1y2 = describe "Animales, alimentos y entrenamientos" $ do
  -- les dejamos estos valores definidos que probablemente les vengan bien
  let tigre = Animal 5 Terrestre 120
  let tigreEntrenado = Animal 0 Terrestre 115
  let lechuza = Animal 40 Volador 10
  let lechuzaEntrenada = Animal 40 Volador 7
  let tiburon = Animal 100 Acuatico 100
  let ningunAnimal :: [Animal]
      ningunAnimal = []
  describe "los de un tipo" $ do
    it "Dada una lista vacia retorna la misma lista vacia" $ do
      losDeTipo Volador [] `shouldBe` ningunAnimal
    it "Retorna los animales de un mismo tipo" $ do
      losDeTipo Volador [tigre, lechuza, tiburon] `shouldBe` [lechuza]
      losDeTipo Terrestre [tigre, lechuza, tiburon,tigreEntrenado] `shouldBe` [tigre,tigreEntrenado]
      losDeTipo Acuatico [tigre, lechuza, tiburon] `shouldBe` [tiburon]
   
  describe "animalesHambrientos" $ do
    it "Dada una lista vacia retorna la misma lista vacia" $ do
      animalesHambrientos [] `shouldBe` ningunAnimal
    it "Retorno solo los animales hambrientos" $ do
      animalesHambrientos [tigre, lechuza, tiburon] `shouldBe` [tigre]
    it "Dada una lista donde no hay animales hambrientos entonces no retorno ninguno" $ do
      animalesHambrientos [lechuza, lechuza, tiburon] `shouldBe` []

  describe "entrenar" $ do
    it "Dada una lista vacia retorna la misma lista vacia" $ do
      animalesHambrientos [] `shouldBe` ningunAnimal
    it "Si el animal es terrestre disminuye el peso y la energia en 5" $ do
      entrenar tigre `shouldBe` tigreEntrenado
    it "Si el animal es volador disminuye el peso  en 3" $ do
      entrenar lechuza `shouldBe` lechuzaEntrenada
    it "Si el animal es acuatico no hace nada porque es re jodido entrenar un animal acuatico." $ do
      entrenar tiburon `shouldBe` tiburon

  describe "alimentos" $ do
    it "Si el animal es alimentado con una baya aumenta su  energía en 5 y el peso en 0.1" $ do
      baya (Animal 5 Terrestre 120) `shouldBe` (Animal 10 Terrestre 120.1) 
    it "Si el animal es alimentado con carnet aumenta su  energía en 20 y el peso en 2" $ do
      carne (Animal 5 Terrestre 120) `shouldBe` (Animal 25 Terrestre 122) 
    
  describe "alimentarATodos" $ do
    it "Recibo un alimento y una lista vacia y retorno una lista vacia" $ do
      alimentarATodos baya [] `shouldBe` []
      alimentarATodos carne [] `shouldBe` []
    it "Recibo una baya y una lista de animales y los alimento a todos" $ do
      alimentarATodos baya [Animal 100 Acuatico 100, Animal 30 Terrestre 20] `shouldBe` [Animal 105 Acuatico 100.1, Animal 35 Terrestre 20.1]  
    it "Recibo una carne y una lista de animales y los alimento a todos" $ do
      alimentarATodos carne [Animal 100 Acuatico 100, Animal 30 Terrestre 20] `shouldBe` [Animal 120 Acuatico 102, Animal 50 Terrestre 22]

  describe "aplicar itinerario" $ do
    it "Dado un animal de [Energia = 25 y Peso = 120] y lo paso por un itinerario vacío retornaré al mismo animal" $ do
      aplicarItinerario  (Animal 25 Terrestre 120) [] `shouldBe`  (Animal 25 Terrestre 120)
    it "Dado un animal de [Energia = 25 y Peso = 120] y lo entreno dos veces sus atributos serán [Energia = 15 y Peso = 110]" $ do
      aplicarItinerario  (Animal 25 Terrestre 120) [entrenar, entrenar] `shouldBe`  Animal 15 Terrestre 110
    it "Dado un animal de [Energia = 25 y Peso = 120] y lo alimento dos veces con bayas sus atributos serán [Energia = 35 y Peso = 120.2]" $ do
      aplicarItinerario  (Animal 25 Terrestre 120) [baya, baya] `shouldBe`  Animal 35 Terrestre 120.2
    it "Dado un animal de [Energia = 25 y Peso = 120] y lo alimento dos veces con carne sus atributos serán [Energia = 65 y Peso = 124]" $ do
      aplicarItinerario  (Animal 25 Terrestre 120) [carne,carne] `shouldBe`  Animal 65 Terrestre 124
    it "Dado un animal de [Energia = 25 y Peso = 120] y lo alimento dos veces, la primera vez con una baya y luego con carne sus atributos serán [Energia =  y Peso = ]" $ do
      aplicarItinerario  (Animal 25 Terrestre 120) [baya,carne] `shouldBe`  Animal 50 Terrestre 122.1
    

suiteDeTestsDeParte3 = describe "Orden Superior" $ do
  describe "mapTupla" $ do
    it "Dada una tupla de elementos del mismo tipo devuelve otra tupla con los resultados de aplicar la funcion a cada valor" $ do
       mapTupla length ("hola", "mundo") `shouldBe` (4,5)
       mapTupla (take 1) ("hola", "mundo") `shouldBe` ("h","m")
       mapTupla (*2) (10, 20) `shouldBe` (20,40)

  describe "menorSegun" $ do
    it "Sea menorSegun acompañada de una funcion y la misma es aplicada a dos parametros retorno el parametro cuyo valor es el MENOR resultante" $ do
      menorSegun length "hola" "mundo" `shouldBe` "hola"
    it "Sea menorSegun acompañada de una funcion de tipo booleana y la misma es aplicada a dos parametros retornaré como menor al valor que sea FALSE" $ do
      menorSegun even 4 3 `shouldBe` 3
    it "Sea menorSegun acompañada de una funcion y la misma es aplicada a dos parametros y el valor resultante es igual entonces retornaré el PRIMER :parametro" $ do
      menorSegun length "hola" "chau" `shouldBe` "hola"
      
  describe "minimoSegun" $ do
    it "Dada una lista vacia y una función, tiene que fallar" $ do
      deberiaFallar (minimoSegun length []) 
    it "Dada una lista y una función, devuelve el menor de toda la lista según la función que se pasó" $ do
      minimoSegun length ["hola", "ornitorrinco", "a"]  `shouldBe` "a"
      minimoSegun energia [tigre, lechuza, tiburon] `shouldBe` Animal 5 Terrestre 120
  
  describe "aplicarVeces" $ do
    it "No se puede  aplicar un indice negativo   " $ do
      deberiaFallar(aplicarVeces (-1) siguiente 1)
    it "No se puede  aplicar un indice igual 0     " $ do
      deberiaFallar(aplicarVeces 0 siguiente 1)
    it "El resultado de aplicar 3 veces la funcion siguiente 1 debe ser 4  " $ do
      aplicarVeces 3 siguiente 1 `shouldBe` 4
    it "El resultado de aplicar 3 veces la funcion potencia definida como (lambda n -> n * 2) y aplicada a 1  debe ser 8  " $ do
      aplicarVeces 3 (\n -> n * 2) 1  `shouldBe` 8
    it "El resultado de aplicar 2 veces la funcion concatenar signo de exclamación definida como (lamda texto -> texto ++ !) y aplicada a hola  debe ser hola!!" $ do
      aplicarVeces 2 (\texto -> texto ++ "!") "hola" `shouldBe` "hola!!"

  describe "replicar" $ do
    it "Si replico 0 veces True el resultado será []" $ do 
      replicar 0  True `shouldBe` []
    it "Si replico 5 veces el numero 1  el resultado será [1,1,1,1,1]" $ do
      replicar 5 1 `shouldBe` [1,1,1,1,1]
    it "Si replico 3 veces la palabra hola el resultado será [hola,hola,hola]" $ do
      replicar 3 "hola" `shouldBe` ["hola", "hola", "hola"]
    it "Si replico 2 veces True el resultado será [True,True]" $ do 
      replicar 2  True `shouldBe` [True, True]

-- suiteDeTestsDeParte4 = describe "combinando funciones" $ do
--   -- Los tests de acá se los dejamos servidos :)
--   describe "|>" $ do
--     it "dado un valor y una funcion, pasa el valor como parametro de la funcion" $ do
--       "hola" |> length `shouldBe` 4
--       3 |> (\n -> n + 2) `shouldBe` 5

--   describe "esVocal" $ do
--     it "dada una letra vocal da True" $ do
--       esVocal 'a' `shouldBe` True
--       esVocal 'E' `shouldBe` True
--     it "dada una letra consontante da False" $ do
--       esVocal 'c' `shouldBe` False
--       esVocal 'B' `shouldBe` False
--     it "dado un caracter que no es una letra da False" $ do
--       esVocal ' ' `shouldBe` False
--       esVocal '@' `shouldBe` False

--   describe "primeraLinea" $ do
--     it "dado un texto sin saltos de linea, devuelve el mismo texto" $ do
--       primeraLinea "hola mundo!" `shouldBe` "hola mundo!"
--     it "dado un texto con saltos de linea, devuelve el mismo hasta antes del primer salto de linea" $ do
--       primeraLinea "hola\nmundo!" `shouldBe` "hola"

--   describe "lasVocales" $ do
--     it "dado un texto vacio, devuelve un string vacio" $ do
--       lasVocales "" `shouldBe` ""
--     it "dado un texto sin vocales, devuelve un string vacio" $ do
--       lasVocales "why" `shouldBe` ""
--     it "dado un texto con vocales, devuelve un string con solo las vocales" $ do
--       lasVocales "chau" `shouldBe` "au"
--       lasVocales "azarath metrion zinthos" `shouldBe` "aaaeioio"

--   describe "contarVocalesDeLaPrimerLinea" $ do
--     it "dado un texto vacio, da 0" $ do
--       contarVocalesDeLaPrimeraLinea "" `shouldBe` 0
--     it "dado un texto sin saltos de linea, da la cantidad de vocales en ese texto" $ do
--       contarVocalesDeLaPrimeraLinea "hello world" `shouldBe` 3
--     it "dado un texto sin saltos de linea, da la cantiad de vocales hasta el primer salto de linea" $ do
--       contarVocalesDeLaPrimeraLinea "Aeea,\n yo soy sabalero" `shouldBe` 4


deberiaFallar :: a -> Expectation
deberiaFallar unaExpresion = evaluate unaExpresion `shouldThrow` anyException

escribime :: Expectation
escribime = expectationFailure "Falta escribir el test"
