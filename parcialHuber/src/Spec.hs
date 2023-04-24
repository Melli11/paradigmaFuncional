module Spec where
import PdePreludat
import Library
import Test.Hspec


correrTests :: IO ()
correrTests = hspec $ do
  suiteDeTestsDeParte1
  suiteDeTestsDeParte2
  -- suiteDeTestsDeParte3

suiteDeTestsDeParte1 = describe "Parte 1: Criterios " $ do

    describe "Criterios de viajes" $ do
      it "el cliente Lucas que vive en Victoria" $ do
        clienteLucas `shouldBe` ("Lucas","Victoria")
      it "Sea un chofer cuyo  criterio de viajes es : aceptar cualquier viaje, entonces aceptará cualquier viaje" $ do
        aceptaCualquierViaje viajeDeEjemplo `shouldBe` True
      it "Sea un chofer que solo acepta los viajes cuyo costo salgan mas de 200, entoces aceptará un viaje cuyo costo es 400." $ do
        aceptaLosViajesMayoresA200 viajeDeCosto400 `shouldBe` True
      it "Sea un chofer que solo acepta los viajes cuyo costo salgan mas de 200, entoces NO aceptará un viaje cuyo costo es 100." $ do
        aceptaLosViajesMayoresA200 viajeDeCosto100 `shouldBe` False
      it "Sea un chofer que solo acepta los viajes de acuerdo a la longitud del nombre del pasajero," $ do
        aceptaViajeDeAcuerdoAlNombreDelCli 4 viajeDeLeoDeFlores  `shouldBe` False
        aceptaViajeDeAcuerdoAlNombreDelCli 4 viajeDeLucasDeVictoria  `shouldBe` True
      it "Sea un chofer que solo acepta los viajes de acuerdo a la zona donde viva el pasajero " $ do
        aceptaViajesQueNOseanDe "Flores" viajeDeLeoDeFlores `shouldBe` False
        aceptaViajesQueNOseanDe "Flores"  viajeDeLucasDeVictoria `shouldBe` True

suiteDeTestsDeParte2 = describe "Parte 2: Viajes y Liquidacion" $ do
    describe "Criterios de viajes" $ do
      it "Dado un chofer y un viaje,el chofer puede realizar el viaje si el viaje cumple con los requisitos" $ do
        puedeTomarViajeUnChofer viajeDeCosto100 choferAlejandra  `shouldBe` True
        puedeTomarViajeUnChofer viajeDeCosto100 choferDaniel  `shouldBe` True
      it "Sea un viaje que no cumple con los requisitos impuestos por el chofer entonces no podrá ser realizado" $ do
        puedeTomarViajeUnChofer viajeDeJuanDeOlivos choferDaniel  `shouldBe` False
      it "La liquidacion de un chofer se calcula como la suma de los costos de sus pasajes" $ do
        liquidacionDeUnChofer choferAlejandra  `shouldBe` 0
        liquidacionDeUnChofer choferDaniel  `shouldBe` 150

-- suiteDeTestsDeParte3 = describe "Realizar un viaje" $ do
--     describe "Los que toman el viaje" $ do
--       it "Dado un viaje y una lista de choferes, filtro aquellos choferes que pueden realizarlo " $ do
--         losQueAceptanElViaje viajeDeCosto100 listaDeChoferes `shouldBe` [choferDaniel,choferAlejandra,choferJorge]
--     -- describe "El que menos viajes tiene" $ do
--     --   it "Dado dos choferes retorno el chofer que menos realizó" $ do
--     --     unChoferTieneMenosViajesQueOtro choferAlejandra choferDaniel  `shouldBe` choferDaniel
--     --   it "Dada una lista de choferes retorno el chofer con menor cantidad de viajes realizados" $ do

    
--     -- describe "Incoporar el viaje a lista de viajes del chofer" $ do
--     --   it "" $ do
        
