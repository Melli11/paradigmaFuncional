module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
    suiteDeTestsDeParte1
    suiteDeTestsDeParte2
    suiteDeTestsDeParte6
   
-- data Auto = Auto {
--  patente :: Patente,
--  desgasteLlantas :: [Desgaste],
--  rpm :: Number,
--  temperaturaAgua :: Number,
--  ultimoArreglo :: Fecha
-- } deriving Show
suiteDeTestsDeParte1 = describe "Punto 1 : Costo de Reparacion " $ do
    let auto_1a = Auto "AT001LN" [] 100 10 (11,08,1990)
    let auto_1b = Auto "DJV214" [] 100 10 (11,08,1990)
    let auto_1c = Auto "DJV215" [] 100 10 (11,08,1990)
    let auto_1d = Auto "DFH029" [] 100 10 (11,08,1990)
    
    describe " Punto 1 " $ do
        it "El costo de reparación de un auto cuya patente es AT001LN es 12500 " $ do
            costoDeReparacion auto_1a `shouldBe` 12500
        it "El costo de reparación de un auto cuya patente es DJV214 es 18000" $ do
            costoDeReparacion auto_1b `shouldBe` 18000
        it "El costo de reparación de un auto cuya patente es DJV215 es 20000" $ do
            costoDeReparacion auto_1c `shouldBe` 20000
        it "El costo de reparación de un auto cuya patente es DFH029 es 15000" $ do
            costoDeReparacion auto_1d `shouldBe` 15000

suiteDeTestsDeParte2 = describe "Punto 2 : Necesita Revision " $ do
    let auto_2a = Auto "AT001LN" [0.5, 0.1, 0.6, 0.4] 100 10 (11,08,2015)
    let auto_2b = Auto "DJV214" [1, 0.1, 0.6, 0.4] 100 10 (11,08,1990)
    let auto_2c = Auto "AT001LN" [0.5, 0.1, 0.6, 0.4] 100 10 (11,08,2016)
    let auto_2d = Auto "DJV214" [1, 0.1, 0.6, 0.4] 100 10 (11,08,2015)
    
    describe " Punto 1 " $ do
        it "Un auto con desgaste de llantas [0.5, 0.1, 0.6, 0.4] no es peligroso " $ do
            elAutoEsPeligroso auto_2a `shouldBe` False
        it "Un auto con desgaste de llantas [1, 0.1, 0.6, 0.4] no es peligroso " $ do
            elAutoEsPeligroso auto_2b `shouldBe` True
        it "Un auto cuyo último arreglo fue en el 2016 no necesita revision " $ do
            necesitaRevisión  auto_2c `shouldBe` False
        it "Un auto cuyo último arreglo fue en el 2015  necesita revision" $ do
            necesitaRevisión auto_2d `shouldBe` True

suiteDeTestsDeParte6 = describe "Punto 6 : Orden Superior " $ do
    let auto_6a = Auto "AT001LN" [0.6, 0.1, 0.6, 0.4] 100 10 (11,08,2015)
    let auto_6b = Auto "DJV214" [0.5, 0.1, 0.6, 0.4] 100 10 (11,08,1990)
    let auto_6c = Auto "DJV215" [0.5, 0.1, 0.6, 0.4] 100 10 (11,08,2016)
    let auto_6d = Auto "DFH029" [1, 0.1, 0.6, 0.4] 100 10 (11,08,2015)
    let tec_6 = [alfa,bravo,charly,tango,zulu,lima]
    let tec_rpta_6a = [bravo,charly,zulu,lima]
    let lista_autos = [auto_6a,auto_6b,auto_6c,auto_6d]
    
    describe " Punto 6 " $ do
        -- it "Sea un auto que tiene de desgaste [0.6, 0.1, 0.6, 0.4] y la lista de tecnicos, solo  [bravo,charly,zulu,lima] lo dejan en condiciones" $ do
        --     dejanElAutoEnCondiciones tec_6 auto_6a `shouldBe` [bravo,charly,zulu,lima]
        it "Sea un auto que tiene 0.5 de desgaste en la primera  si lo evaluamos con la lista de técnicos , todos los tecnicos deberian dejar el auto en condiciones " $ do
            length (dejanElAutoEnCondiciones tec_6 auto_6b) `shouldBe` 6 
        it "Dada una lista de autos cuyas patentes son  cuyas patentes son AT001LN, DJV214, DJV215, DFH029,  donde AT001LN y DFH029" $ do
            costoDelArreglo lista_autos `shouldBe` 27500
