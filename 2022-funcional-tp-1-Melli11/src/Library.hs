module Library where
import PdePreludat

-- 1. Numeros

siguiente :: Number -> Number
siguiente numero = numero + 1

esPositivo :: Number -> Bool
esPositivo numero = numero > 0

-- escriban el tipo de esta función
inversa ::  Number -> Number
inversa numero = 1 / numero

-- 2. Temperaturas

celsiusAFarenheit :: Number -> Number
celsiusAFarenheit gradosCelsius = gradosCelsius * 1.8 + 32

farenheitACelsius :: Number -> Number
farenheitACelsius gradosFarenheit =  (gradosFarenheit - 32) / 1.8

-- escriban el tipo de esta función
haceFrioCelsius :: Number -> Bool
haceFrioCelsius gradosCelsius = gradosCelsius <= 8

-- escriban el tipo de esta función
haceFrioFarenheit :: Number  -> Bool
haceFrioFarenheit gradosFarenheit = haceFrioCelsius (farenheitACelsius gradosFarenheit)

-- 3. Mas numeros!

max' :: Number -> Number -> Number
max' numeroA numeroB    
            | numeroA > numeroB = numeroA
            | otherwise = numeroB

min' :: Number -> Number -> Number
min' numeroA numeroB 
            | numeroA < numeroB = numeroA
            | otherwise = numeroB

-- 4. Pinos

-- escriban el tipo de esta función

valorDeCorte  :: Number
valorDeCorte = 3
kgsPorCmHastaValorDeCorte :: Number
kgsPorCmHastaValorDeCorte = 3
kgsPorCmSuperiorAlvalorDeCorte :: Number
kgsPorCmSuperiorAlvalorDeCorte = 2
centrimetros :: Number
centrimetros = 100 


pesoPino :: Number -> Number
pesoPino alturaPino 
            | alturaPino <= valorDeCorte = kgsPorCmHastaValorDeCorte * centrimetros * alturaPino -- Priorizo el caso donde el pino es menor a 3 mts
            | otherwise = conversionAlturaPeso  valorDeCorte kgsPorCmHastaValorDeCorte kgsPorCmSuperiorAlvalorDeCorte alturaPino  centrimetros   --Abarco toda la altura del pino

conversionAlturaPeso :: Number -> Number -> Number -> Number -> Number -> Number
conversionAlturaPeso valorDeCorte kgsPorCmHastaValorDeCorte kgsPorCmSuperiorAlvalorDeCorte altura centimetros = (metrosDelPinoHastaElValorDeCorte altura valorDeCorte) * kgsPorCmHastaValorDeCorte * centimetros + (metrosDelPinoSuperiorAlValorDeCorte altura valorDeCorte) * kgsPorCmSuperiorAlvalorDeCorte * centimetros 

metrosDelPinoHastaElValorDeCorte :: Number -> Number -> Number
metrosDelPinoHastaElValorDeCorte altura valorDeCorte = min' altura valorDeCorte

metrosDelPinoSuperiorAlValorDeCorte :: Number -> Number -> Number
metrosDelPinoSuperiorAlValorDeCorte altura valorDeCorte = altura - valorDeCorte 


-- escriban el tipo de esta función

esPesoUtil :: Number -> Bool
esPesoUtil pesoPino = pesoPino >= 400 && pesoPino  <= 1000 --Valido el rango del peso del pino

-- escriban el tipo de esta función
sirvePino :: Number -> Bool
sirvePino alturaPino = esPesoUtil (pesoPino alturaPino) -- En el  () obtengo el peso de un pino a partir de su altura y con la funcion esPesoUtil determino si el pino es util o no en funcion de su peso