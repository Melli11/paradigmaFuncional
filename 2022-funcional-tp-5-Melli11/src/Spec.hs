module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
    suiteDeTestsDeParte1
    suiteDeTestsDeParte2
    suiteDeTestsDeParte3

suiteDeTestsDeParte1 = describe "Parte 1 : Hamburguesas" $ do
  let hamburguesaConCarne = Hamburguesa 10 [Pan,Carne,Cheddar,Pan]
  let hamburguesaConDobleCarne = Hamburguesa 10 [Carne,Pan,Carne,Cheddar,Pan]
  let hamburguesaConPollo = Hamburguesa 10 [Pan,Pollo,Cheddar,Pan]
  let hamburguesaConDoblePollo = Hamburguesa 10 [Pollo,Pan,Pollo,Cheddar,Pan]
  let hamburguesaConPolloyCarne = Hamburguesa 10 [Pan,Pollo,Carne,Cheddar,Pan]
  let hamburguesaConMasPolloyCarne = Hamburguesa 10 [Pollo,Pan,Pollo,Carne,Cheddar,Pan]
  let hamburguesaConInflacion = Hamburguesa 5 []
  let ningunIngrediente :: [Ingrediente]
      ningunIngrediente = []


  describe "Agrandar" $ do
    it "Dada una hamburguesa cuyo ingrediente base es carne,si lo agrando le agrego otro medallon de  carne" $ do
       agrandar hamburguesaConCarne `shouldBe` hamburguesaConDobleCarne
    it "Dada una hamburguesa cuyo ingrediente base es pollo, si lo agrando le agrego otro medallon mas pollo" $ do
       agrandar hamburguesaConPollo `shouldBe` hamburguesaConDoblePollo
    it "Dada una hamburguesa mixta,es decir con carne y pollo,como es indistinto,entonces si lo agrando le agrego otro medallon mas de pollo" $ do
       agrandar hamburguesaConPolloyCarne `shouldBe` hamburguesaConMasPolloyCarne
  describe "Agregar ingrediente" $ do
    it "Dada una hamburguesa y un ingrediente, agrego el ingrediente como primero a la lista de ingredientes de la hamburguesa " $ do
       agregarIngrediente Panceta hamburguesaConCarne `shouldBe` Hamburguesa 10 [Panceta,Pan,Carne,Cheddar,Pan]
    it "Dada una hamburguesa sin ingredientes y un ingrediente, agrego el ingrediente como primero a la lista de ingredientes de la hamburguesa " $ do
       agregarIngrediente Pan hamburguesaConInflacion `shouldBe` Hamburguesa 5 [Pan]
  describe "Descuento" $ do
    it "Dada una hamburguesa con ingredientes entonces le aplico el descuento" $ do
       descuento 50 hamburguesaConCarne `shouldBe` Hamburguesa 5 [Pan,Carne,Cheddar,Pan]
    it "Dada una hamburguesa sin ingredientes entonces le aplico el descuento" $ do
       descuento 50 hamburguesaConInflacion `shouldBe` Hamburguesa 2.5 []

suiteDeTestsDeParte2 = describe "Parte 2 : Algunas hamburguesas mas" $ do
  let cuartoDeLibraConMasCarneYCheddar = Hamburguesa 20 [Carne,Cheddar,Pan,Carne,Cheddar,Pan]
  let bigPdepConPromoDelDia = (agregarIngrediente Papas . descuento 30 . agregarIngrediente Curry) dobleCuarto
  let dobleCuartoConPromoDelDia = (agregarIngrediente Papas . descuento 30 ) dobleCuarto
--   let 

  describe "dobleCuarto" $ do
    it "Un doble Cuarto: [Carne,Cheddar,Pan,Carne,Cheddar,Pan] NO ES IGUAL a  un cuarto de libra [Pan,Carne,Cheddar,Pan]  " $ do
       dobleCuarto /= cuartoDeLibra `shouldBe` True
    it "Un doble Cuarto es un cuarto de libra con mas carne y cheddar " $ do
       dobleCuarto `shouldBe` Hamburguesa {precioBase = 20, ingredientes = [Carne,Cheddar,Pan,Carne,Cheddar,Pan]}
    it "El precio final de un doble cuarto es 84  " $ do
       precio dobleCuarto `shouldBe` 84
  describe "BigPdep" $ do
    it "Un BigPdep es un doble cuarto con curry " $ do
       bigPdep `shouldBe` agregarIngrediente Curry  dobleCuarto
    it "El precio final de un BigPdep es 89  " $ do
       precio bigPdep `shouldBe` 89
  describe "delDia" $ do
    it "Dada una bigPdep con promo del dia entonces le aplico la promoción: le agrego papas y un descuento del 30% " $ do
       delDia bigPdep `shouldBe` bigPdepConPromoDelDia
    it "El precio final de un BigPdep con promo es 93  " $ do
       precio (delDia bigPdep) `shouldBe` 93
    it "Dado un dobleCuarto con promo del dia entonces le aplico la promoción: le agrego papas y un descuento del 30% " $ do
       delDia dobleCuarto `shouldBe` dobleCuartoConPromoDelDia
    it "El precio final de un dobleCuarto con promo es 88  " $ do
       precio (delDia dobleCuarto) `shouldBe` 88


suiteDeTestsDeParte3 = describe "Parte 3 : Algunas cambios mas" $ do
  let hamburguesaSinQueso = Hamburguesa 20 [Pan, Carne,Pan]
  let hamburguesaSinQuesoVeggie = Hamburguesa 20 [PatiVegano,Pan,Pan]
  let hamburguesaRara = Hamburguesa 20 [Pan, Cheddar, Carne, Cheddar, Pan, Cheddar, Pan]
  let hamburguesaRaraVeggie = Hamburguesa 20 [PatiVegano,QuesoDeAlmendras,QuesoDeAlmendras,QuesoDeAlmendras,Pan,Pan,Pan]

  describe "hacerVeggie" $ do
    it "Sea un dobleCuarto, si la hago veggie, la devuelvo con PatyVegano y cambio todos los queso cheddar por queso de almendras" $ do
       hacerVeggie (Hamburguesa 20 [Carne,Cheddar,Pan,Carne,Cheddar,Pan]) `shouldBe` Hamburguesa 20 [PatiVegano,PatiVegano,QuesoDeAlmendras,QuesoDeAlmendras,Pan,Pan]
    it "El precio final de un doble cuarto transformado a veggie es: Precio base (20) + Paty Vegano (10) + Paty Vegano (10) + Queso Almendras (15) + Queso Almendras (15) + Pan (2) + Pan (2)  = 74  " $ do
       precio (Hamburguesa 20 [PatiVegano,PatiVegano,QuesoDeAlmendras,QuesoDeAlmendras,Pan,Pan]) `shouldBe` 74
    it "Sea una hamburguesa de carne sin queso cheddar, si la hago veggie, solo la devuelvo con PatyVegano " $ do
       hacerVeggie hamburguesaSinQueso `shouldBe` hamburguesaSinQuesoVeggie
    it "El precio final de una hamburguesa super veggie es: Precio base (20) + Paty Vegano (10) + Pan (2) + Pan (2) = 34  " $ do
       precio hamburguesaSinQuesoVeggie `shouldBe` 34
    it "Sea una hamburguesa rara con tres quesos [Pan, Cheddar, Carne, Pan, Cheddar, Pan] entonces reemplazo c/u de sus quesos por Queso de Almendra" $ do
       hacerVeggie hamburguesaRara `shouldBe` hamburguesaRaraVeggie

  describe "cambiarPanDePati" $ do
    it "Sea un dobleCuarto, si cambio de pan, la devuelvo con pan integral y el resto de ingredientes permanece igual" $ do
       cambiarPanDePati (Hamburguesa 20 [Carne,Cheddar,Pan,Carne,Cheddar,Pan]) `shouldBe` Hamburguesa 20 [PanIntegral,PanIntegral,Carne,Cheddar,Carne,Cheddar]
    it "Sea una hamburguesa al plato, sin pan , la devuelvo igual , sin pan " $ do
       cambiarPanDePati (Hamburguesa 20 [Carne,Cheddar,Carne,Cheddar]) `shouldBe` Hamburguesa 20 [Carne,Cheddar,Carne,Cheddar]
    it "Sea una hamburguesa del chavo del ocho, solo de pan, entonces la devuelvo con la misma cantidad de panes pero integrales" $ do
       cambiarPanDePati (Hamburguesa 20 [Pan,Pan,Pan,Pan,Pan]) `shouldBe` Hamburguesa 20 [PanIntegral,PanIntegral,PanIntegral,PanIntegral,PanIntegral]

  describe "dobleCuartoVegano" $ do
    it "Sea un dobleCuarto si lo hago vegano entonces cambiaré sus medallones por patyVegano, el cheddar por queso de Almedras y el pan por pan integral" $ do
       dobleCuartoVegano (Hamburguesa 20 [Carne,Cheddar,Pan,Carne,Cheddar,Pan]) `shouldBe` Hamburguesa 20 [PatiVegano,PatiVegano,QuesoDeAlmendras,QuesoDeAlmendras,PanIntegral,PanIntegral]
    it "Sea un dobleCuarto si lo hago vegano entonces su precio total será : Precio base (20) + Paty Vegano (10) + Paty Vegano (10) + Queso Almendras (15) + Queso Almendras (15) + PanIntegral (3) + PanIntegral (3) = 76  " $ do
       precio (Hamburguesa 20 [PatiVegano,PatiVegano,QuesoDeAlmendras,QuesoDeAlmendras,PanIntegral,PanIntegral]) `shouldBe` 76
   