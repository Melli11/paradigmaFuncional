{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  suiteDeTestsDeParte1
  suiteDeTestsDeParte2

suiteDeTestsDeParte1 = describe  "Punto 2: Estrategias FMI" $ do
  describe "Prestarle n Millones a un Pais" $ do
    it "Si el Fmi presta n millones a un pais, el pais aumenta su deuda en un 150% " $ do
       prestarleNMillones 10 (Pais 5 10 10 [Mineria, Petroleo] 0) `shouldBe` Pais 5 10 10 [Mineria, Petroleo] 15 
  describe "Reducir Sector Publico" $ do
    it "Reducir X cantidad de puestos de trabajo: reduce los puestos activos del sector publico, si son mas de 100 disminuye el ingreso per capita en 20" $ do
       reducirSectorPublicoEn 101 (Pais 100 200 10 [Mineria, Petroleo] 0)  `shouldBe` (Pais 80 99 10 [Mineria, Petroleo] 0)
    it "Reducir X cantidad de puestos de trabajo: reduce los puestos activos del sector publico, si son menos de 100 disminuye el ingreso per capita en 15" $ do
       reducirSectorPublicoEn 100 (Pais 100 100 10 [Mineria, Petroleo] 0)  `shouldBe` (Pais 85 0 10 [Mineria, Petroleo] 0)
  describe "Darle una empresa al FMI" $ do
    it "Darle una empresa afin de la explotación de algun recurso natual al FMI implica dejar sin ese recurso al pais y ademas reduce la deuda en 2 millones, si no tiene deuda tiene saldo a favor" $ do
       darEmpresaAlFmi Mineria (Pais 100 200 10 [Mineria, Petroleo] 0)  `shouldBe` (Pais 100 200 10 [Petroleo] (-2000000))
    it "Darle una empresa afin de la explotación de algun recurso natual al FMI implica dejar sin ese recurso al pais y ademas reduce la deuda en 2 millones" $ do
       darEmpresaAlFmi Petroleo (Pais 100 200 10 [Mineria, Petroleo] 2000000)  `shouldBe` (Pais 100 200 10 [Mineria] 0)
  describe "Establecer Blindaje" $ do
    it "Si se establece un blindaje sobre el pais X: entonces el pais tomara deuda por la mitad de su PBI y perdera 500 puestos de trabajo del sector publico" $ do
       establecerBlindaje (Pais 10 5 5 [Petroleo] 0)  `shouldBe` (Pais 10 (-495) 5 [Petroleo] 75)  
    it "El PBI del el pais X: se calcula como el ingreso per cápita multiplicado por su población activa, sumando puestos públicos y privados de trabajo" $ do
       productoBrutoInterno (Pais 10 5 5 [Petroleo] 0)  `shouldBe` 100
    it "La poblacion activa de un pais X se calcula sumando puestos públicos y privados de trabajo" $ do
       poblacionActiva (Pais 0 5 5 [Petroleo] 0)  `shouldBe` 10

suiteDeTestsDeParte2 = describe  "Punto 2: Orden Superior" $ do
  let listadoDePaisesQuePuedenSafar = [Pais {ingresoPerCapita = 4140, sectorPublico = 400, sectorPrivado = 650000, recursos = [Petroleo,Ecoturismo], deuda = 30000000},Pais {ingresoPerCapita = 4140, sectorPublico = 400, sectorPrivado = 650000, recursos = [Petroleo,Ecoturismo], deuda = 40000000},Pais {ingresoPerCapita = 5, sectorPublico = 10, sectorPrivado = 10, recursos = [Mineria,Petroleo], deuda = 10000000}]
  describe "Paises que pueden Safar" $ do
    it "Dada una lista de paises pueden zafar los que tienen Petroleo entre sus riquezas naturales" $ do
       puedenZafar [brasil,venezuela,bolivia,argentina,namibia] `shouldBe` listadoDePaisesQuePuedenSafar 
  describe "Total de deuda" $ do
     it "El total de la Deuda de un conjunto de paises , es saldo a favor del FMI" $ do
       totalDeDeuda  listaDePaises `shouldBe` 150000000
       
    
