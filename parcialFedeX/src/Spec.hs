module Spec where
import PdePreludat
import Library
import Test.Hspec


-- envioX = UnEnvio origen   Destino  Peso PrecioBase [Categorias]
-- envioX = UnEnvio ("CABA", "ARG") ("TUC","ARG") 10 1000 ["Consola"]

correrTests :: IO ()
correrTests = hspec $ do
  describe "Cargos" $ do
    it "Cargo Categorico: Si la categoria coincide se aplica un cargo de %50 sobre el precio base" $ do
      precioBase (cargoCategorico "Consola" 50 envioARG ) `shouldBe` 500
    it "Cargo por Sobrepedo: Si el peso es menor a un peso X no afecto el precio " $ do
      precioBase (cargoPorSobrePeso 11 envioARG) `shouldBe` 1000
    it "Cargo por Sobrepedo: Si el peso es MAYOR a un peso X modifico el precio  " $ do
      precioBase (cargoPorSobrePeso 9 envioARG) `shouldBe` 1080
    it "Cargo arbitrario: Agrego $50 al precio del envio " $ do
      precioBase (cargoArbitrario envioARG) `shouldBe` 1050
