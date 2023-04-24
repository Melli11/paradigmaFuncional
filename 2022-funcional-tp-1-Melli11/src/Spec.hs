module Spec where
import PdePreludat
import Library
import Test.Hspec
import           Control.Monad (unless)

correrTests :: IO ()
correrTests = hspec $ do
  suiteDeTestsDeParteI
  suiteDeTestsDeParteII
  suiteDeTestsDeParteIII
  suiteDeTestsDeParteIV
  
suiteDeTestsDeParteI =
  describe "Parte I: Numeros" $ do

    describe "siguiente" $ do
      it "el siguiente de un numero es el numero + 1" $ do
        siguiente (-1) `shouldBe` 0
        siguiente 0 `shouldBe` 1
        siguiente 1 `shouldBe` 2

    describe "inversa" $ do
      it "la inversa de 1 es 1" $ do
        inversa 1 `shouldBe` 1
      it "la inversa de cualquier numero es el resultado de dividir 1 por ese numero" $ do
        inversa 4 `shouldBe` 0.25
        inversa 0.25 `shouldBe` 4

    describe "esPositivo" $ do
      it "es verdad para los numeros mayores a 0" $ do
        esPositivo 1 `shouldBe` True
      it "es falso para los numeros menores a 0" $ do
        esPositivo (-1) `shouldBe` False
      it "es falso para el 0" $ do
        esPositivo 0 `shouldBe` False

suiteDeTestsDeParteII =
  describe "Parte 2: Temperaturas" $ do

    describe "celsiusAFarenheit" $ do
      it "pasa una temperatura de celsius a farenheit" $ do
        celsiusAFarenheit (-40) `shouldBeEqualUpTo2Decimals` (-40)
        celsiusAFarenheit (-18) `shouldBeEqualUpTo2Decimals` (-0.4)
        celsiusAFarenheit 0 `shouldBeEqualUpTo2Decimals` 32
        celsiusAFarenheit 10 `shouldBeEqualUpTo2Decimals` 50

    describe "farenheitACelsius" $ do
      it "pasa una temperatura de farenheit a celsius" $ do
        farenheitACelsius (-40) `shouldBeEqualUpTo2Decimals` (-40)
        farenheitACelsius (-0.4) `shouldBeEqualUpTo2Decimals` (-18)
        farenheitACelsius 32  `shouldBeEqualUpTo2Decimals` 0
        farenheitACelsius 50 `shouldBeEqualUpTo2Decimals` 10

    it "farenheitACelsius y celsiusAFarenheit son inversas (usar una y luego la otra da el numero original)" $ do
      farenheitACelsius (celsiusAFarenheit (-40)) `shouldBeEqualUpTo2Decimals` (-40)
      farenheitACelsius (celsiusAFarenheit (-0.4)) `shouldBeEqualUpTo2Decimals` (-0.4)
      farenheitACelsius (celsiusAFarenheit 32) `shouldBeEqualUpTo2Decimals` 32
      farenheitACelsius (celsiusAFarenheit 50) `shouldBeEqualUpTo2Decimals` 50

    describe "haceFrioCelsius" $ do
      it "es verdad cuando hacen menos de 8 grados celsius" $ do
        haceFrioCelsius 7 `shouldBe` True
      it "es verdad cuando hacen exactamente 8 grados celsius" $ do
        haceFrioCelsius 8 `shouldBe` True
      it "es falso cuando hacen mas de 8 grados celsius" $ do
        haceFrioCelsius 9 `shouldBe` False

    describe "haceFrioFarenheit" $ do
      it "es verdad cuando hacen menos de 8 grados celsius (46.4 F)" $ do
        haceFrioFarenheit 45 `shouldBe` True
      it "es verdad cuando hacen exactamente 8 grados celsius (46.4 F)" $ do
        haceFrioFarenheit 46.4 `shouldBe` True
      it "es falso cuando hacen mas de 8 grados celsius (46.4 F)" $ do
        haceFrioFarenheit 47 `shouldBe` False

suiteDeTestsDeParteIII =
  describe "Parte 3: Mas Numeros!" $ do

    describe "max'" $ do
      it "si el primero es mayor al segundo devuelve el primero" $ do
        max' 2 1 `shouldBe` 2
      it "si el primero es menor al segundo devuelve el segundo" $ do
        max' 1 2 `shouldBe` 2
      it "si ambos son iguales, devuelve ese valor" $ do
        max' 1 1 `shouldBe` 1

    describe "min'" $ do
      it "si el primero es mayor al segundo devuelve el segundo" $ do
        min' 2 1 `shouldBe` 1
      it "si el primero es menor al segundo devuelve el primero" $ do
        min' 1 2 `shouldBe` 1
      it "si ambos son iguales, devuelve ese valor" $ do
        min' 1 1 `shouldBe` 1
        
suiteDeTestsDeParteIV =
  describe "Parte 4: Pinos" $ do

    describe "pesoPino" $ do
      it "el peso de un pino de hasta 3 metros, es 3kg por centimetro" $ do
        pesoPino 2 `shouldBe` 600
      it "el peso de un pino de 3 metros, es 3kg por centimetro" $ do
        pesoPino 3 `shouldBe` 900
      it "el peso de un pino de mas de 3 metros, es 3kg por centimetro para los primeros 3 metros, y 2kg por centimetro por cada uno de los metros restantes" $ do
        pesoPino 4 `shouldBe` 1100

    describe "esPesoUtil" $ do
      it "es falso si el peso es menor a 400" $ do
        esPesoUtil 399 `shouldBe` False
      it "es verdad si el peso es 400" $ do
        esPesoUtil 400 `shouldBe` True
      it "es verdad si el peso está entre 400 y 1000" $ do
        esPesoUtil 700 `shouldBe` True
      it "es verdad si el peso es 1000" $ do
        esPesoUtil 1000 `shouldBe` True
      it "es falso si el peso es más de 1000" $ do
        esPesoUtil 1001 `shouldBe` False

    describe "sirvePino" $ do
      it "es verdad si el peso del pino le es util a la fabrica" $ do
        sirvePino 2 `shouldBe` True
      it "es falso si el peso del pino no le es util a la fabrica" $ do
        sirvePino 1 `shouldBe` False
        sirvePino 5 `shouldBe` False

shouldBeEqualUpTo2Decimals :: Number -> Number -> Expectation
shouldBeEqualUpTo2Decimals aNumber anotherNumber = shouldBeEqualWithErrorLessThan 0.01 aNumber anotherNumber

shouldBeEqualWithErrorLessThan :: Number -> Number -> Number -> Expectation       
shouldBeEqualWithErrorLessThan error aNumber anotherNumber
  | aNumber - anotherNumber < error = pure () -- Esto hace que el test de verde!
  | otherwise = expectationFailure (show aNumber ++ " no es igual (comparando con error < " ++ show error ++ ") a " ++ show anotherNumber)
