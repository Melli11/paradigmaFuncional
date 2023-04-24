module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "irAlaPlaya" $ do
    it "Sea un turista como Sasuke que viaja solo, entonces bajará su cansancio en 5 unidades" $ do
      irAlaPlaya (Turista 100 50 True ["Japones","Español"]) `shouldBe` Turista (100-5) 50 True ["Japones","Español"]
    it "Sea un turista meloso que no viaja solo, entonces bajará su stress en 1 unidad" $ do
      irAlaPlaya (Turista 4 10 False ["Castellano"]) `shouldBe` Turista 4 (10-1) False ["Castellano"]
  describe "salirAHablarUnIdioma" $ do
    it "Sea un turista que sale a hablar un idioma entonces el turista aprende el idioma y ademas termina acompañado" $ do
      salirAHablarUnIdioma "Aleman" (Turista 4 10 True ["Castellano"]) `shouldBe` Turista 4 10 False ["Aleman","Castellano"]
  describe "apreciarAlgunElementoDelPaisaje" $ do
    it "Sea un turista que contempla el mar, entonces rudicirá su nivel de stress en 3 unidades" $ do
      apreciarAlgunElementoDelPaisaje "mar" (Turista 4 10 False ["Castellano"]) `shouldBe` Turista 4 (10-3) False ["Castellano"]
    it "Sea un turista que contempla la nada misma, entonces rudicirá su nivel de stress en 0 unidades" $ do
      apreciarAlgunElementoDelPaisaje "" (Turista 4 10 False ["Castellano"]) `shouldBe` Turista 4 10 False ["Castellano"]
  describe "caminar" $ do
    it "Sea un turista que camina una cantidad X de minutos , aumentará su cansancio pero reducirá su stress " $ do
      caminar 20 (Turista 4 10 False ["Castellano"]) `shouldBe` Turista (4+5) (10-5) False ["Castellano"]
    it "Sea un turista vago que no camina  entonces no cambiará su cansancio ni su stress " $ do
      caminar 0 sasuke `shouldBe` sasuke
  describe "paseo en barco" $ do
    it "Si un turista realiza un paseo con la marea FUERTE en barco aumentará su stress en 6 unidades y el cansancio en 10 " $ do
      paseoEnBarco  Fuerte (Turista 4 10 False ["Castellano"]) `shouldBe` Turista (4+10) (10+6) False ["Castellano"]
    it "Si un turista realiza un paseo con la marea MODERADA , no se verá afectado " $ do
      paseoEnBarco  Moderada sasuke `shouldBe` sasuke
    it "Si un turista realiza un paseo con la marea TRANQUILA, el turista hace sociales, camina 10', aprecia la vista del mar y sale a hablar aleman" $ do
      paseoEnBarco  Tranquila (Turista 4 10 False ["Castellano"]) `shouldBe` Turista 6 4 False ["aleman","Castellano"]
  describe "deltaExcursionSegun" $ do
    it "La función deltaExcursionSegun indica la variacion del estado de una persona luego de hacer una excursion" $ do
      deltaExcursionSegun stress ana irAlaPlaya `shouldBe` -3
  describe "laExcursionEsEducativa" $ do
    it "Una excursion es educativa para una persona si luego de realizarla aprende un idioma" $ do
       esUnaExcursionEsEducativa (Turista 4 10 False ["Castellano"]) (salirAHablarUnIdioma "frances")   `shouldBe` True 
    it "Una excursion NO ES educativa para una persona si luego de realizarla NO aprende un idioma" $ do
       esUnaExcursionEsEducativa (Turista 4 10 False ["Castellano"]) (caminar 10)  `shouldBe` False 
  describe "Excursion desestresante" $ do
    it "Una excursion es desestresante si luego de realizarla le redujo al menos 3 unidades de stress" $ do
       esUnaExcursionDesestresante (Turista 100 50 True ["Japones","Español"]) (apreciarAlgunElementoDelPaisaje "terremoto") `shouldBe` True
    it "Una excursion NO es desestresante si luego de realizarla NO le redujo al menos 3 unidades de stress" $ do
       esUnaExcursionDesestresante (Turista 100 5 True ["Japones","Español"]) (apreciarAlgunElementoDelPaisaje "es")  `shouldBe` False 
  