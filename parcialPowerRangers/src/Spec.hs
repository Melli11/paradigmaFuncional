module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  suiteDeTestsDeParte1
  suiteDeTestsDeParte2
  suiteDeTestsDeParte4
  suiteDeTestsDeParte5
  suiteDeTestsDeParte6
  suiteDeTestsDeParte8
suiteDeTestsDeParte1 = describe "Parte 1: Personas y PowerRangers" $ do
    let tomy = (["Galan","Poderoso"], True )
    let tomyMalo = (["Galan","Poderoso"], False )
    let rangerHabilidoso = (Verde, ["Galan","Poderoso","Confiable","Habil","Metodico","Nocturno"], 10 )

    describe "Es buena persona" $ do
      it "Dada una persona con bondad entonces se cumple que ES BUENA persona" $ do
        esBuenaPersona  tomy `shouldBe` True
      it "Dada una persona con maldad entonces se cumple que NO ES BUENA persona" $ do
        esBuenaPersona  tomyMalo `shouldBe` False
    describe "Nivel de Pelea de un Ranger" $ do
      it "Dado un ranger con nivel de pelea 10 entonces se cumple su poder es 10" $ do
        nivelDePelea rangerHabilidoso `shouldBe` 10
suiteDeTestsDeParte2 = describe "Parte 2: de Persona a Ranger " $ do
    let jony = (["Espiritu libre","Vago"], True )
    let casuality = ([], False )
    describe "convertirEnPowerRanger" $ do
      it "Dada una persona y un color, entonces convierto a la persona en un Ranger de ese color" $ do
        convertirEnPowerRanger Verde jony `shouldBe` (Verde,["SuperEspiritu libre","SuperVago"],18)
        convertirEnPowerRanger Rojo casuality `shouldBe` (Rojo,[],0)
      it "Dada una lista de habilidades las puedo potenciar  " $ do
        potenciarHabilidades jony  `shouldBe` ["SuperEspiritu libre","SuperVago"]
        potenciarHabilidades casuality `shouldBe` []
      it "Dada una personsa le doy un nivel de pelea acorde a la cantidad de letras de sus habilidades " $ do
        darNivelDePelea jony `shouldBe` 18
        darNivelDePelea casuality `shouldBe` 0

suiteDeTestsDeParte4 = describe "Parte 4: Buscando al lider " $ do
    let jony = (["Espiritu libre","Vago"], True )
    let casuality = ([], False )
    let listaDeRangers = [(Verde,["Patadura","Olvidadizo"],20),(Rosa,["Carisma","Mandona"],5),(Rojo,["Horrible","Necio"],10)]
    let listaDeRangers2 = [(Amarillo,["Horrible","Necio"],10),(Verde,["Patadura","Olvidadizo"],20),(Rosa,["Carisma","Mandona"],5)]
    describe "findOrElse" $ do
      it "Dada una condición, un valor y una lista, devuelve el primer elemento  que cumpla la condición, o el valor dado si ninguno la cumple" $ do
        findOrElse even 3 [1,7,11] `shouldBe` 3
        findOrElse even 3 [1,7,2] `shouldBe` 2
        findOrElse ("a"==) "b" ["a","c","d"] `shouldBe` "a"
        findOrElse ("a"==) "b" ["x","c","d"] `shouldBe` "b"
      it "Dada una lista de Rangers que tiene en su equipo al Ranger Rojo entonces él será el lider" $ do
        rangerLider listaDeRangers  `shouldBe` (Rojo,["Horrible","Necio"],10)
      it "Dada una lista de Rangers que NO tiene en su equipo al Ranger Rojo entonces  el lider será el primero de la lista , en este caso el Amarillo" $ do
        rangerLider listaDeRangers2 `shouldBe` (Amarillo,["Horrible","Necio"],10)

suiteDeTestsDeParte5 = describe "Parte 5: El mejor " $ do
    describe "findOrElse" $ do
      it " dada una lista, y una función que tome un elemento y devuelva un valor ordenable, encuentra el máximo de la misma." $ do
        maximumBy (>1) [1,2,3,4,5,6] `shouldBe` True
        maximumBy (max 1) [1,2,3,4,5,6] `shouldBe` 6

suiteDeTestsDeParte6 = describe "Parte 6: El habilidoso " $ do
    let elHabilidoso = (Verde, ["Galan","Poderoso","Confiable","Habil","Metodico","Nocturno"], 10 )
    let elNoHabilidoso = (Amarillo, ["Galan","Poderoso","Confiable"], 20 )
    
    describe "Habilidades de un Ranger" $ do
      it "Dado un Ranger retorno su lista de habilidades " $ do
        habilidadesRanger elHabilidoso `shouldBe` ["Galan","Poderoso","Confiable","Habil","Metodico","Nocturno"]
    describe "Es un Ranger Habilidoso" $ do
      it "Dado un Ranger es habilidoso si tiene mas de 5 habilidades  " $ do
        rangerHabilidoso elHabilidoso `shouldBe` True
      it "Dado un Ranger que no tiene mas de 5 habilidades entonces no es habilidoso  " $ do
        rangerHabilidoso elNoHabilidoso `shouldBe` False

suiteDeTestsDeParte8 = describe "Parte 8: Chicas Superpoderosas" $ do
    let escuadronSuperPoderoso = [bellota,bombon,burbuja]
    let escuadronMedioPelo = [mojojo,bellota,bombon,mojojo]

    let bellota = (Verde, 10)
    let bombon = (Amarillo,5)
    let burbuja = (Rojo,30)
    let mojojo = (Negro, 100)

    describe "chicaLider" $ do
      it "Dado un escuadron de chicas superpoderosas que tiene en su equipo a la chica Roja entonces su lider sera ella " $ do
        chicaLider escuadronSuperPoderoso `shouldBe` burbuja
      it "Dado un escuadron de chicas superpoderosas que NO tiene en su equipo a la chica Roja entonces su lider sera la cabeza del equipo " $ do
        chicaLider escuadronMedioPelo `shouldBe` mojojo
        

