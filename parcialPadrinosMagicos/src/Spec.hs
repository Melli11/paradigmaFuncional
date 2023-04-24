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
suiteDeTestsDeParte1 = describe "Concediendo Deseos" $ do

  describe "Aprender habilidades" $ do
    it "Sea una nueva habilidad llamada Conducir entonces un chico aprenderá una nueva habilidad " $ do
      length (habilidad (aprenderHabilidades ["Conducir"] chicoSinHabilidadCon2Deseos)) `shouldBe`  1
    it "Sea un chico sin habilidad si no le enseño nada seguira siendo un inutil" $ do
      length (habilidad (aprenderHabilidades [] chicoSinHabilidadCon2Deseos)) `shouldBe` 0 
    it "Sea un chico con habilidad si no le enseño nada seguira con la misma habilidad" $ do
      length (habilidad (aprenderHabilidades [] chicoConHabilidad)) `shouldBe` 1 
  describe "Mayoria de Edad" $ do
    it "Sea un niño y le cumplo el deseo de ser Mayor entonces tendrá 18" $ do
      edad (serMayor chico) `shouldBe` 18 

suiteDeTestsDeParte2 = describe "Padrinos magicos" $ do
  let chicoSinHabilidadCon2Deseos = Chico "bobi" 10 [] [aprenderHabilidades ["Andar en bicicleta"],serMayor]
  let chicoConPrimerDeseoCumplido = Chico "bobi" 10 ["Andar en bicicleta"] [serMayor]
  describe "Wanda" $ do
    it "Dado un chico, wanda le cumple el primer deseo " $ do
      length (habilidad (wanda chicoSinHabilidadCon2Deseos)) `shouldBe`  1
    it "Dado un chico, wanda lo hace madurar  " $ do
      edad (wanda chicoSinHabilidadCon2Deseos) `shouldBe`  11
  describe "Cosmo" $ do
    it "Dado un chico, cosmo lo hace desmadurar, quedando con la mitad de años de edad" $ do
      edad (cosmo chicoSinHabilidadCon2Deseos) `shouldBe`  5
    it "Como cosmo es olvidadizo, no le concede ningún deseo." $ do
      length (habilidad (cosmo chicoSinHabilidadCon2Deseos)) `shouldBe`  0
  describe "muffinMagico" $ do
    it "Dado un chico le concede todos sus deseos, ser mayor y aprender a bicicletear." $ do
      length (habilidad(muffinMagico chicoSinHabilidadCon2Deseos)) `shouldBe` 1 
      edad (muffinMagico chicoSinHabilidadCon2Deseos) `shouldBe`  18 

suiteDeTestsDeParte3 = describe "En busqueda de pareja" $ do
  describe "tieneHabilidad" $ do
    it "Tiene habilidad : Dado un chico y una habilidad, el chico posee esa habilidad" $ do
      tieneHabilidad "Conducir" chicoPeligroso `shouldBe` True
    it "No Tiene esa habilidad : Dado un chico y una habilidad, el chico no posee dicha habilidad" $ do
      tieneHabilidad "Nadar" chicoPeligroso `shouldBe`  False
  describe "esSuperMaduro" $ do
    it "Es super maduro si el chico es mayor de edad y ademas sabe manejar" $ do
      esSuperMaduro taxiDriver `shouldBe` True
    it "Nene de mama: no es super maduro, tiene 18 anios, pero no sabe manejar" $ do
      esSuperMaduro (Chico "Bebote" 18 [] [] ) `shouldBe` False
    it "Nene de mama: no es super maduro, sabe manejar pero no tiene 18 años" $ do
      esSuperMaduro (Chico "Bebu Jr" 10 ["Sabe manejar"] [] ) `shouldBe` False
  describe "quienConquistaA unaChica losPretendientes" $ do
    it "Dada una chica y una lista de pretendientes, devuelve al que se queda con la chica, es decir, el primero que cumpla con la condición que ella quiere" $ do
       nombre (quienConquistaA vicky pretendientes) `shouldBe` "bobi"
    it "Si no hay ninguno que la cumpla, devuelve el último pretendiente" $ do
       nombre (quienConquistaA vicky pretendientesFinal) `shouldBe` "bobiDosDeseos"

suiteDeTestsDeParte4 = describe "infractoresDeDaRules" $ do
  let galan = Chico "Tarzan" 25 [] [aprenderHabilidades ["enamorar"]]
  let asesinofallido = Chico "Chapa de plastico" 20 [] [aprenderHabilidades ["matar"],aprenderHabilidades ["cantar"],aprenderHabilidades ["saltar"],aprenderHabilidades ["remar"],aprenderHabilidades ["imitar"],aprenderHabilidades ["nadar"]]
  let asesino = Chico "Ted Bundy" 30 [] [aprenderHabilidades ["dominar el mundo"],aprenderHabilidades ["secuestrar"],aprenderHabilidades ["enganar"],aprenderHabilidades ["sociabilizar"],aprenderHabilidades ["matar"]]
  let chicosConDeseosProhibidos = [galan,taxiDriver,chico,asesino] --deberia ser ok
  let chicosConDeseosNoProhibidos = [taxiDriver,chico] --deberia no retornar nombres

  describe "Auxiliar:Deseos prohibidos" $ do
    it "Dado el chico con el sexto deseo prohibido: el deseo no es prohibido porque no se agrega dentro las 5 primeras habilidades" $ do
       algunoConDeseosProhibidos asesinofallido `shouldBe` False
    it "Dado el chico varios deseos prohibidios: el deseo es prohibido porque se agrega dentro las 5 primeras habilidades y que no están permitidas" $ do
       algunoConDeseosProhibidos galan `shouldBe` True
  describe "quienConquistaA unaChica losPretendientes" $ do
    it "Dada  una  lista  de  chicos,  devuelve  la  lista  de  los  nombres  de  aquellos que tienen deseos prohibidos" $ do
       infractoresDeDaRules chicosConDeseosProhibidos `shouldBe` ["Tarzan","Ted Bundy"]
    it "Si no hay ninguno que la cumpla, devuelve el último pretendiente" $ do
       infractoresDeDaRules chicosConDeseosNoProhibidos `shouldBe` []
