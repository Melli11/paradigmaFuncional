module Library where
import PdePreludat


data Cuenta = Cuenta {
    idCuenta :: String,
    saldo :: Number
}deriving (Show)

type Transaccion = Cuenta -> Cuenta
type Id = Cuenta -> String
type Bloque = [(Id,Transaccion)]
type Blockchain = [Bloque]



transaccionMineria :: Cuenta -> Cuenta
transaccionMineria cuenta = cuenta { saldo = saldo cuenta + 25}

transaccionIntercambio :: Number -> Cuenta -> Cuenta
transaccionIntercambio  coinsCuentaA cuentaB = cuentaB { saldo = saldo cuentaB +  coinsCuentaA}  

--

{-Pto 1-}
funcionDeCuenta ::   Number -> Transaccion
funcionDeCuenta numero cuenta = cuenta { saldo =  saldo cuenta + numero} 

funcionDeCobranza :: Number -> Transaccion 
funcionDeCobranza numero cuenta = funcionDeCuenta ((+ numero) .saldo $cuenta) cuenta 

funcionDePago :: Number -> Transaccion
funcionDePago numero cuenta = funcionDeCuenta ( max 0 . (flip (-) numero).saldo $cuenta  ) cuenta 

funcionDeMineria :: Transaccion 
funcionDeMineria = transaccionMineria

{-Pto 2-}

funcionDeBusqueda ::  Cuenta -> String -> Bool
funcionDeBusqueda  cuenta identificador =  idCuenta cuenta == identificador 

funcionDeBusquedaLista :: (Cuenta->Bool) -> [Cuenta] -> Cuenta 
funcionDeBusquedaLista condicion listaDeCuentas = (head.filter) (== condicion) $listaDeCuentas

funcionDeBusquedaListaDos ::  (Cuenta->Bool) -> [Cuenta] -> [Cuenta]
funcionDeBusquedaListaDos condicion listaDeCuentas = filter (/= condicion ) $listaDeCuentas


{-Pto 3-}


funcionModificar :: String -> [Cuenta] -> (Cuenta -> Cuenta) -> [Cuenta] 
funcionModificar identificador listaDeCuentas funcionModificadoraDeCuenta = funcionModificadoraDeCuenta (map (funcionDeBusqueda identificador) listaDeCuentas) :listaDeCuentas     


{-Pto 4-}

transaccionesEjecutadas :: Bloque -> [Cuenta] -> [Cuenta]
transaccionesEjecutadas bloque listaDeCuentas = fold


{-Pto 5-}

saldoMayorACero :: Cuenta -> Bool
saldoMayorACero  = (>0).saldo 

laCuentaEsEstable :: [Cuenta] -> Bool
laCuentaEsEstable listaDeCuentas = all (saldoMayorACero) $listaDeCuentas

{-Pto 6-}

{-
type Bloque = [(Id,Transaccion)]
type Blockchain = [Bloque]
-}
{-}
chequeoBlockChainEsEstable :: Blockchain -> [Cuenta] -> Bool
chequeoBlockChainEsEstable [unBlock] listaDeCuentas  = filter (laCuentaEsEstable) . map (snd.unBlock) $listaDeCuentas 
--chequeoBlockChainEsEstable (CabezaBlock:ColaBlock) listaDeCuentas  = 
-}
{- Pto 7-}


--funcionSinPudor :: [[a1]] -> (a1 -> Number, Number -> Number) -> Number -> Number
funcionSinPudor :: [[a1]] -> (a1 -> Number, a2 -> a2) -> a2 -> a2
funcionSinPudor x y 
    | (length . filter even . map (fst y) $ head x) > 10 = id 
    | otherwise = snd y

{-
Rta:

La  funcionSinPudor tiene 3 parametros uno de ellos está dado de forma implicita que corresponde a la segunda componente de la tupla.
Y es una tupla, porque tiene aplicada la función fst.
X es una lista de listas ya que se la aplica la función head ,que es una función típica del reportorio de listas, y luego es aplicada como un parametro de la función map.
La primer componente de la tupla de Y es un elemento genérico cuya imagen es un numérico a su vez lo podemos inferir
porque está relacionado en la composición de funciones que esta en la primer  guarda (length . filter even)  
La función funcionSinPudor retorna el tipo del segundo elemento de la tupla de Y que es un genérico.

-}
