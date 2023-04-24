{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
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
        suiteDeTestsDeParte4

suiteDeTestsDeParte1 = describe "Parte 1: Calculos " $ do

    describe "1a. distancia" $ do
        it "La distancia entre el punto A de coordenadas (1,1,1) y el punto B de coordenadas (1,1,1) deberia ser 0." $ do
            distancia (1,1,1) (1,1,1) `shouldBe` 0
    describe "1b. tiempoRequerido" $ do
        it "El tiempo requerido para ir a los planetas namek y solaris,  que están en las posiciones (1,1,1) y (100,100,100) respectivamente y viajando a una velocidad de 1000 será de  0.17147303." $ do
            tiempoRequerido 1000 namek solaris `shouldBe` 0.17147303
    describe "Aux: funcionVectorial" $ do
        it "Aplica una funcion a todos los elementos de un vector de 3 posiciones.\n" $ do
            funcionVectorial (-) (1,1,1) (1,1,1)  `shouldBe` (0,0,0)

suiteDeTestsDeParte2 = describe "Parte 2: Terricola y tiempo  " $ do
 
    describe "2.transitarAnios" $ do
        it "Dado un astronauta que tiene una edad de 30 años y pasa 10 años en la tierra (factor de conversion 1 a 1) entonces  su edad final será 40." $ do
            edadTerrestre (transitarAnios terricola 10) `shouldBe` 40
        it "El terricola no cambiara su nombre." $ do
            nombre (transitarAnios terricola 10) `shouldBe` "Terricola"
        it "Dado un astronauta que tiene una edad de 50 años y pasa  10 años en el planetaX (factor de conversion 1 a 0.5) entonces  su edad final será  55.\n" $ do
            edadTerrestre (transitarAnios viajero 10) `shouldBe` 55

suiteDeTestsDeParte3 = describe "Parte 3: Naves y viajes " $ do

    describe "3.Realizar viajes " $ do
        it "Si un astronauta de edad 30 y destino PlanetaX  realiza un viaje en una nave vieja con 5 tanques de oxigeno, aumentará su edad a 79 y monedas." $ do
            edadTerrestre (realizarViaje terricola planetaX unaNaveViejaDeCincoTanques) `shouldBe` 79.487165931
        it "Si un astronauta de edad 30 y destino PlanetaX  realiza un viaje en una nave vieja con 8 tanques de oxigeno, aumentará su edad a 64 y monedas, pues la nave es mas rapida." $ do
            edadTerrestre (realizarViaje terricola planetaX unaNaveViejaDeOchoTanques) `shouldBe` 64.641016151
        it "Si un astronauta de edad 30 y destino PlanetaX  realiza un viaje en una nave vieja con 5 tanques de oxigeno,su planeta de destino se actualizará a PlanetaX." $ do
            nombrePlaneta (planetaActual (realizarViaje terricola planetaX unaNaveViejaDeCincoTanques)) `shouldBe` "planetaX"
        it "Si un astronauta de edad 30 y destino Solaris  realiza un viaje en una nave futurista no cambiara su edad, pues la nave es tan rapida que llegara instantaneamente al destino elegido." $ do
            edadTerrestre (realizarViaje terricola solaris unaNaveFuturista) `shouldBe` 30
        it "Si un astronauta de edad 30 y destino Solaris  realiza un viaje en una nave futurista su planeta de destino se actualizará a Solaris.\n" $ do
            nombrePlaneta (planetaActual (realizarViaje terricola solaris unaNaveViejaDeCincoTanques)) `shouldBe` "solaris"

suiteDeTestsDeParte4 = describe "Parte 4: Rescatistas y Rescatados " $ do

    describe "4a.realizarViajeDeRescate" $ do
        it "Sea un grupo de tres rescatistas [astroboy,megaman,martian], que viajan en una nave vieja de 5 tanques de oxigeno, y realizan un viaje de ida desde Namek para rescatar a un astronauta varado en el Planeta X. Entonces el grupo de rescatistas actualizara su planeta de destino a Planeta X" $ do
            map (nombrePlaneta .  planetaActual) (rescatistas (viajeDeIda ryan equipoDeRescateConNaveViejaCincoTanques)) `shouldBe` ["planetaX","planetaX","planetaX"]    
        it "Sea un grupo de tres rescatistas [astroboy,megaman,martian] cuyas edades son [20,22,23] respectivamente ,que viajan en una nave vieja de 5 tanques de oxigeno , y realizan un viaje de ida desde Namek para rescatar a un astronauta varado en el Planeta X. Entonces actualizaran su edad en [32.124355653,34.124355653,35.124355653]" $ do
            map edadTerrestre (rescatistas (viajeDeIda ryan equipoDeRescateConNaveViejaCincoTanques)) `shouldBe` [32.124355653,34.124355653,35.124355653]    
        it "Sea un grupo de tres rescatistas [astroboy,megaman,martian] cuyas edades son [20,22,23] respectivamente ,que viajan en una nave vieja de 8 tanques de oxigeno , y realizan un viaje de ida desde Namek para rescatar a un astronauta varado en el Planeta X. Entonces actualizaran su edad en [28.487048957,30.487048957,31.487048957], es logico pues la nave es mas rapida" $ do
            map edadTerrestre (rescatistas (viajeDeIda ryan equipoDeRescateConNaveViejaDeOchoTanques)) `shouldBe` [28.487048957,30.487048957,31.487048957]   
        it "Sea un grupo de tres rescatistas [astroboy,megaman,martian] cuyas edades son [20,22,23] y  viajan en una nave futurista, y realizan un viaje de ida desde Namek y van a rescatar a un astronauta varado en el Planeta X.Entonces su edad serán la misma que la del inicio del viaje" $ do
            map edadTerrestre (rescatistas (viajeDeIda ryan equipoDeRescateConNaveFuturista)) `shouldBe` [20,22,23]
        it "Sea un grupo de tres rescatistas [astroboy,megaman,martian] y van a socorrer a un astronauta varado de nombre Ryan, el nuevo grupo de astronautas será de 4 personas [Ryan,Astro,Megaman,Martian]" $ do
            map nombre  (rescatistas (agregarAlEquipoDeRescate ryan equipoDeRescateConNaveFuturista)) `shouldBe` ["Ryan","Astro","Megaman","Martian"]
        it "Sea el grupo de tres rescatistas,astronautas que viajan en una nave vieja, y realizan un viaje de regreso desde el planeta donde está varado el astronauta y se dirigen al planeta inicial con el astronauta varado\n.Entonces actualizaran su planeta de destino al planeta de origen" $ do
            map (nombrePlaneta .  planetaActual) (viajeDeRegreso equipoDeRescateConNaveViejaCincoTanques equipoDeRescateConNaveViejaDeOchoTanquesFinal) `shouldBe` ["namek","namek","namek","namek"]    
        it "Dado un grupo de rescate entonces realizar un viaje de rescate implica: agregar al pasajero varado a la lista de astronautas, aumentarle la edad a los tripulantes, con los viajes de ida y vuelta, y el planeta de destino final sera el mismo que el de planeta de partida\n  " $ do
            map edadTerrestre (realizarViajeDeRescate ryan equipoDeRescateConNaveViejaCincoTanques) `shouldBe` [37.124355653,44.248711306,46.248711306,47.248711306]    
        
    describe "4b.nombresDeAstronautasRescatables" $ do
        it "Sea un grupo de rescatistas, todos menores a 90 años que parten a socorrer a un astronauta varado con edad menor a 90, entonces,el varado PUEDE SER rescatado si luego de realizar el viaje de rescate TODOS los tripulantes tendran menos de 90 años" $ do
            puedeSerRescatado equipoDeRescateConNaveViejaCincoTanques ryan `shouldBe` True
        it "Sea un grupo de rescatistas, donde hay alguno con edad superior a 90 años y  parten a socorrer a un astronauta varado con edad menor a 90,entonces,el varado NO PUEDE SER rescatado porque luego de realizar el viaje de rescate todos NO tendran menos de 90 años" $ do
            puedeSerRescatado (GrupoRescatistas [astroOld,megaman,martian] unaNaveViejaDeCincoTanques) ryan `shouldBe` False
        it "Sea un grupo de rescatistas, todos con edad menor  a 90 años y  parten a socorrer a un astronauta varado con edad de 89 años, entonces,el varado NO PUEDE SER rescatado porque luego de realizar el viaje de rescate todos NO tendran menos de 90 años, pues el viaje aumenta la edad del astronauta varado" $ do
            puedeSerRescatado (GrupoRescatistas [ryan,megaman,martian] unaNaveViejaDeCincoTanques) astroOld `shouldBe` False
        it "Sea un grupo de rescatistas, todos menores a 90 años que parten a socorrer a un grupo de UN astronauta varado  con edad menor a 90 y de nombre RYAN, entonces la lista de nombres es: [Ryan]" $ do
            nombresDeAstronautasRescatables equipoDeRescateConNaveViejaCincoTanques [ryan] `shouldBe` ["Ryan"]
        it "Sea un grupo de rescatistas, todos menores a 90 años que parten a socorrer a un grupo de astronautas varados los cuales todos tienen una edad superior a 90  entonces no podremos rescatar a nadie " $ do
            nombresDeAstronautasRescatables equipoDeRescateConNaveViejaCincoTanques listaDeAstronautasOld `shouldBe` []
        it "Sea un grupo de rescatistas, todos menores a 90 años que parten a socorrer a un grupo de astronautas varados los cuales tiene edades variadas entonces la lista de rescatables es [Cerebro,Tommy,Jazz] " $ do
            nombresDeAstronautasRescatables equipoDeRescateConNaveViejaCincoTanques [astroOld,pinki,cerebro,tommy,jazz,megamanOld] `shouldBe` ["Cerebro","Tommy","Jazz"]
    