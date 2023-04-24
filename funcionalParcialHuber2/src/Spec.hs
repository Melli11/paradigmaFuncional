module Spec where
import PdePreludat
import Library
import Test.Hspec
sinViajes :: [Viaje]
sinViajes = []

correrTests :: IO ()
correrTests = hspec $ do

-- PUNTO 3 A --
  describe "Punto 3a - Test Cliente Lucas" $ do
    it "El cliente Lucas vive en Victoria" $ do
      clienteLucas `shouldBe` ("Lucas","Victoria")

{-

data Viaje = Viaje {
    fecha :: (Number,Number,Number),
    cliente :: Cliente,
    costo :: Number
} deriving (Show)

data Chofer = Chofer {
    nombre :: Nombre,
    kilometraje :: Number,
    viajes :: [Viaje],
    condicionViaje :: Viaje -> Bool
} deriving (Show)


-}
-- PUNTO 3 B --
  describe "Punto 3b - Test Chofer Daniel" $ do
    it "El  chofer se llama Daniel y su auto tiene 23500 km" $ do
      nombre daniel `shouldBe` "Daniel" 
      kilometraje daniel `shouldBe` 23500 
    it "El chofer Daniel cuyo auto tiene 23500 kms, viajo con Lucas "$ do
      (head. map fst . map cliente) (viajes daniel) `shouldBe` "Lucas"
      (head. map snd . map cliente) (viajes daniel) `shouldBe` "Victoria"
    it "El chofer Daniel cuyo auto tiene 23500 kms, viajo con Lucas el 20/04/2017 "$ do
      (head. map fecha)  (viajes daniel) `shouldBe` (20,4,2017)
    it "El chofer Daniel cuyo auto tiene 23500 kms, viajo con Lucas el 20/04/2017 por un valor de 150"$ do
      (head. map costo)  (viajes daniel) `shouldBe` 150
    it "El chofer Daniel no toma viajes hacia Olivos"$ do
       condicionViaje daniel viajeAOlivos `shouldBe` False 
       condicionViaje daniel viajeEjemplo `shouldBe` True 


-- PUNTO 3 C --
  describe "Punto 3c - Test Chofer Alejandra" $ do
    it "El  chofer se llama Alejandra y su auto tiene 180000 km" $ do
      nombre alejandra `shouldBe` "Alejandra" 
      kilometraje alejandra `shouldBe` 180000 
    it "El chofer Alejandra cuyo auto tiene 180000 km, no hizo viajes " $ do
      (null.viajes) alejandra  `shouldBe` True
    it "El chofer Alejandra cuyo auto tiene 180000 km, no hizo viajes y toma cualquier viaje " $ do
      condicionViaje alejandra viajeEjemplo `shouldBe` True 

-- PUNTO 4 --
  describe "Punto 4 - Puede tomar el viaje" $ do
    it "Un chofer puede tomar un viaje si se cumplen sus condiciones " $ do
      puedeTomarElViaje viajeEjemplo alejandra `shouldBe` True
      puedeTomarElViaje viajeAOlivos daniel `shouldBe` False
      puedeTomarElViaje viajeEjemplo daniel `shouldBe` True

-- PUNTO 5 --
  describe "Punto 5 - Liquidación de un chofer" $ do
    it "La liquidacion de Alejandra es de 0" $ do
      liquidacionDeUnChofer alejandra `shouldBe` 0
    it "La liquidacion de Daniel es de 150" $ do
      liquidacionDeUnChofer daniel `shouldBe` 150

-- PUNTO 7 --
  describe "Punto 7 - Puede tomar viaje Nito" $ do
    it "Nito puede tomar el viaje de Lucas" $ do
      puedeTomarElViaje viajeDeLucas nitoInfyC  `shouldBe` True