module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
  describe "obtenerElemento" $ do -- Les dejo este de regalo
    it "retorna el elemento que esta en la posicion pasada" $ do
      obtenerElemento 1 [1, 2, 3] `shouldBe` 2
    it "falla si se pasa una posicion negativa" $ do
      deberiaFallar (obtenerElemento (-1) [1]) 
    it "falla si se pasa una lista vacia" $ do
      deberiaFallar (obtenerElemento 0 [])  
    it "falla si se pasa una posicion para la cual no hay elemento" $ do
      deberiaFallar (obtenerElemento 2 [1]) 

  describe "sacarHastaEncontrar" $ do 
    it "Si busco una carta que NO ESTA en el mazo retorno el mismo mazo " $ do 
      sacarHastaEncontrar cartaUnoVerde [CartaNumerica 0 Azul, CartaNumerica 1 Azul, CartaNumerica 5 Amarillo] `shouldBe` [CartaNumerica 0 Azul, CartaNumerica 1 Azul, CartaNumerica 5 Amarillo]
    it "Si busco la cartaUnoVerde en el mazo [cartaCeroAzul,cartaUnoAzul,cartaUnoVerde,cartaCincoAmarillo] retorno el mazo hasta encontrar cartaUnoVerde" $ do 
      sacarHastaEncontrar cartaUnoVerde [CartaNumerica 0 Azul, CartaNumerica 1 Azul, CartaNumerica 1 Verde, CartaNumerica 5 Amarillo] `shouldBe` [CartaNumerica 0 Azul, CartaNumerica 1 Azul, CartaNumerica 1 Verde]
    it "Si busco una carta que está primera en el mazo retornaré la primera " $ do
      sacarHastaEncontrar cartaUnoVerde [CartaNumerica 1 Verde, CartaNumerica 0 Azul, CartaNumerica 1 Azul, CartaNumerica 1 Verde, CartaNumerica 5 Amarillo] `shouldBe` [CartaNumerica 1 Verde]
    it "Si busco una carta que está en el ultimo lugar retornaré todo el mazo " $ do 
      sacarHastaEncontrar cartaUnoVerde [CartaNumerica 0 Azul, CartaNumerica 1 Azul, CartaNumerica 2 Verde, CartaNumerica 5 Amarillo,CartaNumerica 1 Verde] `shouldBe` [CartaNumerica 0 Azul, CartaNumerica 1 Azul, CartaNumerica 2 Verde, CartaNumerica 5 Amarillo,CartaNumerica 1 Verde]
    it "Si busco una carta que está repetida en el mazo retornare el mazo hasta encontrar la primera carta repetida  " $ do 
      sacarHastaEncontrar cartaUnoVerde [CartaNumerica 0 Azul, CartaNumerica 1 Azul, CartaNumerica 1 Verde, CartaNumerica 1 Verde] `shouldBe` [CartaNumerica 0 Azul, CartaNumerica 1 Azul, CartaNumerica 1 Verde]
    
  describe "lasRojas" $ do
    it "Dado un mazo SIN cartas rojas voy a retornar un mazo sin cartas " $ do
      lasRojas [CartaNumerica 5 Verde,CartaNumerica 0 Azul, CartaNumerica 1 Azul, CartaNumerica 1 Verde,CartaNumerica 2 Verde,CartaNumerica 1 Verde] `shouldBe` []
    it "Dado un mazo con TODAS cartas rojas voy a retornar el mazo completo" $ do
      lasRojas [CartaNumerica 5 Rojo,CartaNumerica 0 Rojo, CartaNumerica 1 Rojo, CartaNumerica 1 Rojo,CartaNumerica 2 Rojo,CartaNumerica 1 Rojo] `shouldBe` [CartaNumerica 5 Rojo,CartaNumerica 0 Rojo, CartaNumerica 1 Rojo, CartaNumerica 1 Rojo,CartaNumerica 2 Rojo,CartaNumerica 1 Rojo]
    it "Del siguiente mazo solo numerico [cartaCincoRoja,cartaCeroAzul,cartaUnoAzul,cartaUnoVerde,cartaDosRojo,cartaUnoRoja] voy a retornar las cartas rojas [cartaCincoRoja,cartaDosRoja,cartaUnoRoja] " $ do
      lasRojas [CartaNumerica 5 Rojo,CartaNumerica 0 Azul, CartaNumerica 1 Azul, CartaNumerica 1 Verde,CartaNumerica 2 Rojo,CartaNumerica 1 Rojo] `shouldBe` [CartaNumerica 5 Rojo,CartaNumerica 2 Rojo,CartaNumerica 1 Rojo]
    it "Del siguiente mazo con algunas especiales [cartaReversaRoja,cartaCeroAzul,cartaEspecialAzul,cartaUnoVerde,cartaSaltarTurnoRoja,cartaMas4Roja] voy a retornar las cartas rojas [cartaReversaRoja,cartaSaltarTurnoRoja,cartaMas4Roja] " $ do
      lasRojas [CartaEspecial Reversa Rojo,CartaNumerica 0 Azul, CartaEspecial Reversa Azul, CartaNumerica 1 Verde,CartaEspecial SaltarTurno Rojo,CartaEspecial Mas4 Rojo] `shouldBe` [CartaEspecial Reversa Rojo,CartaEspecial SaltarTurno Rojo,CartaEspecial Mas4 Rojo]
    it "Del siguiente mazo  mixto [cartaReversaRoja,cartaCeroAzul,cartaEspecialAzul,cartaUnoVerde,cartaCuatroRoja] voy a retornar las cartas rojas [cartaReversaRoja,cartaCuatroRoja] " $ do
      lasRojas [CartaEspecial Reversa Rojo,CartaNumerica 0 Azul, CartaEspecial Reversa Azul, CartaNumerica 1 Verde,CartaNumerica 4 Rojo] `shouldBe` [CartaEspecial Reversa Rojo,CartaNumerica 4 Rojo]
   
  describe "lasQueSonDeColor" $ do
    it "Sea una busqueda de una carta de color Amarillo/Verde/Azul/Rojo en un mazo sin cartas  voy a retornar un mazo sin cartas" $ do
        lasQueSonDeColor Azul [] `shouldBe` []
        lasQueSonDeColor Rojo [] `shouldBe` []
        lasQueSonDeColor Amarillo [] `shouldBe` []
        lasQueSonDeColor Verde [] `shouldBe` []
    it "Sea una busqueda de una carta azul en el mazo: [cartaReversaAmarilla,cartaMas4Verde,cartaCincoRoja,cartaCeroAzul,cartaUnoAzul,cartaUnoVerde,cartaDosRojo,cartaUnoRoja] solo voy a retornar las que son del color Azul" $ do
        lasQueSonDeColor Azul [CartaEspecial Reversa Amarillo, CartaEspecial Mas4 Verde,CartaNumerica 5 Rojo,CartaNumerica 0 Azul, CartaNumerica 1 Azul, CartaNumerica 1 Verde,CartaNumerica 2 Rojo,CartaNumerica 1 Rojo] `shouldBe` [CartaNumerica 0 Azul, CartaNumerica 1 Azul]
    it "Sea una busqueda de una carta azul en siguiente mazo sin cartas azules [cartaReversaAmarilla,cartaMas4Verde,cartaCincoRoja,cartaUnoVerde,cartaDosRojo,cartaUnoRoja] no voy a retornar ninguna carta" $ do
        lasQueSonDeColor Azul [CartaEspecial Reversa Amarillo, CartaEspecial Mas4 Verde,CartaNumerica 5 Rojo, CartaNumerica 1 Verde,CartaNumerica 2 Rojo,CartaNumerica 1 Rojo] `shouldBe` []    
    it "Sea una busqueda de una carta verde en el mazo[cartaReversaAmarilla,cartaMas4Verde,cartaCincoRoja,cartaCeroAzul,cartaUnoAzul,cartaUnoVerde,cartaDosRojo,cartaUnoRoja] solo voy a retornar las que son del color Verde" $ do
        lasQueSonDeColor Verde [CartaEspecial Reversa Amarillo, CartaEspecial Mas4 Verde,CartaNumerica 5 Rojo,CartaNumerica 0 Azul, CartaNumerica 1 Azul, CartaNumerica 1 Verde,CartaNumerica 2 Rojo,CartaNumerica 1 Rojo] `shouldBe` [CartaEspecial Mas4 Verde,CartaNumerica 1 Verde]
    it "Sea una busqueda de una carta verde en siguiente mazo sin cartas verdes [cartaReversaAmarilla,cartaCincoRoja,cartaCeroAzul,cartaUnoAzul,cartaDosRojo,cartaUnoRoja] no voy a retornar ninguna carta" $ do
        lasQueSonDeColor Verde [CartaEspecial Reversa Amarillo,CartaNumerica 5 Rojo,CartaNumerica 0 Azul, CartaNumerica 1 Azul,CartaNumerica 2 Rojo,CartaNumerica 1 Rojo] `shouldBe` []
    it "Sea una busqueda de una carta amarilla en siguiente mazo [cartaReversaAmarilla,cartaMas4Verde,cartaCincoRoja,cartaCeroAzul,cartaUnoAzul,cartaCuatroAmarilla,cartaDosRojo,cartaUnoRoja] solo voy a retornar las que son del color Amarillo" $ do
        lasQueSonDeColor Amarillo [CartaEspecial Reversa Amarillo, CartaEspecial Mas4 Verde,CartaNumerica 5 Rojo,CartaNumerica 0 Azul, CartaNumerica 1 Azul, CartaNumerica 4 Amarillo,CartaNumerica 2 Rojo,CartaNumerica 1 Rojo] `shouldBe` [CartaEspecial Reversa Amarillo,CartaNumerica 4 Amarillo]
    it "Sea una busqueda de una carta amarilla en siguiente mazo sin cartas amarillas [CartaMas4Verde,cartaCincoRoja,cartaCeroAzul,cartaUnoAzul,cartaDosRojo,cartaUnoRoja] no voy a retornar ninguna carta" $ do
        lasQueSonDeColor Amarillo [CartaEspecial Mas4 Verde,CartaNumerica 5 Rojo,CartaNumerica 0 Azul, CartaNumerica 1 Azul,CartaNumerica 2 Rojo,CartaNumerica 1 Rojo] `shouldBe` []
    it "Sea una busqueda de una carta roja en siguiente mazo [cartaCincoRoja,cartaCeroAzul,cartaUnoAzul,cartaUnoVerde,cartaDosRojo,cartaUnoRoja] solo voy a retornar las cartas que son del color Rojo " $ do
        lasQueSonDeColor Rojo [CartaNumerica 5 Rojo,CartaNumerica 0 Azul, CartaNumerica 1 Azul, CartaNumerica 1 Verde,CartaNumerica 2 Rojo,CartaNumerica 1 Rojo] `shouldBe` [CartaNumerica 5 Rojo,CartaNumerica 2 Rojo,CartaNumerica 1 Rojo]
    it "Sea una busqueda de una carta roja en siguiente mazo sin cartas rojas [cartaCeroAzul,cartaUnoAzul,cartaUnoVerde] no voy a retornar ninguna carta " $ do
        lasQueSonDeColor Rojo [CartaNumerica 0 Azul, CartaNumerica 1 Azul, CartaNumerica 1 Verde] `shouldBe` []

  describe "lasFiguras" $ do
    it "Sea un mazo sin cartas voy a retornar otro mazo sin cartas" $ do
        lasFiguras [] `shouldBe` []
    it "Del siguiente mazo mixto [cartaMas4Rojo,cartaTresRojo,cartaNueveVerde,cartaCeroAzul,cartaReversaRoja] solo voy a retornar [cartaMas4Rojo,cartaReversaRoja] " $ do
        lasFiguras [CartaEspecial Mas4 Rojo,CartaNumerica 3 Rojo,CartaNumerica 9 Verde,CartaNumerica 0 Azul,CartaEspecial Reversa Rojo] `shouldBe` [CartaEspecial Mas4 Rojo,CartaEspecial Reversa Rojo]
    it "Del siguiente mazo numerico [cartaMas4Rojo,cartaTresRojo,cartaNueveVerde,cartaCeroAzul,cartaSieteRoja] NO voy a retornar ninguna carta" $ do
        lasFiguras [CartaNumerica 4 Rojo,CartaNumerica 3 Rojo,CartaNumerica 9 Verde,CartaNumerica 0 Azul,CartaNumerica 7 Rojo] `shouldBe` []
    it "Sea una unica carta especial y se encuentra en el principio del mazo , retorno solo esa carta " $ do
        lasFiguras [CartaEspecial Mas4 Rojo,CartaNumerica 3 Rojo,CartaNumerica 9 Verde,CartaNumerica 0 Azul] `shouldBe` [CartaEspecial Mas4 Rojo]
    it "Sea una unica carta especial y se encuentra en el final del mazo , retorno solo esa carta " $ do
        lasFiguras [CartaNumerica 3 Rojo,CartaNumerica 9 Verde,CartaNumerica 0 Azul,CartaEspecial Reversa Rojo] `shouldBe` [CartaEspecial Reversa Rojo]
  describe "sumatoria" $ do
    it "De la siguiente lista numerica [10,10,10,10,10] voy a sumar todos sus elementos y su total será 50" $ do
        sumatoria [10,10,10,10,10] `shouldBe` 50 
    it "De la siguiente lista numerica [-10,-10,-10,-10,-10] voy a sumar todos sus elementos y su total será -50" $ do
        sumatoria [-10,-10,-10,-10,-10] `shouldBe` -50
    it "De la siguiente lista vacia [] voy a sumar todos sus elementos y su total será 0" $ do
        sumatoria [] `shouldBe` 0
-- F
-- Funcion auxiliar definida para testear en obtenerElemento
deberiaFallar :: a -> Expectation
deberiaFallar unaExpresion = evaluate unaExpresion `shouldThrow` anyException

escribime :: Expectation
escribime = expectationFailure "Falta escribir el test"
