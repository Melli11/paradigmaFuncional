# Currycoin

Ahora que están tan de moda las criptomonedas, nosotros vamos a modelar la propia... Basta de esas farsas de BitCoin, Ethereum o Dogecoin... es hora de **Currycoin**.

Para poder modelar nuestra criptomoneda, necesitamos algunas cosas fundamentales, el bloque que conforma cada parte de la blockchain y los estados de las cuentas.

**Las cuentas** están formadas por el identificador de la cuenta, un String, y el saldo, un número que nos dice cuántos Currycoins tiene la cuenta.

Cada **bloque** es una lista de transacciones. **Las transacciones** afectan a las cuentas, y pueden ser una transacción de minería, que le suma 25 Currycoins a una cuenta, o una transacción de intercambio, que hace que pasen una cantidad de Currycoins X de una cuenta a otra.
Finalmente, **las blockchains**, no son más que una lista de bloques.

**Se prohíbe el uso de recursividad, salvo que se aclare lo contrario.**


1. 
    1. Modelar los tipos de los bloques, las transacciones y las cuentas.  
    **Nota:** Tener en cuenta lo comentado en el siguiente punto para las transacciones.
    2. Las transacciones... Para simplificar el modelo, el intercambio estará separado en transacciones de cobro y pago:
        1. Hacer una función de pago, que reciba un número y una cuenta, y devuelva la cuenta con esa cantidad menos de Currycoins.
        2. Hacer una función de cobranza, que reciba un número y una cuenta, y devuelva la cuenta con esa cantidad más de Currycoins.
        3. Hacer una función de minería, que reciba una cuenta y la devuelva con 25 Currycoins más.
        Nota: No repetir lógica
2. Necesitamos funciones que busquen una cuenta en una lista de cuentas:
    1. Hacer una función que dado un identificador y una cuenta, nos indique si el identificador corresponde a esa cuenta.
    2. Hacer una función que dada una condición y una lista de cuentas, devuelva la primera cuenta que cumple la condición.
    3. Hacer una función que dada una condición y una lista de cuentas, devuelva la lista de cuentas SIN la primera cuenta que cumpla la condición.
3. Hacer una función que reciba un identificador, una lista de cuentas y una función que modifique una cuenta. De manera que devuelva la lista de cuentas, pero con la cuenta correspondiente al identificador modificada por la función. Las demás cuentas deben permanecer sin cambios.
**Nota:** No hace falta conservar el orden original de la lista

4. Queremos saber como un bloque afecta a una lista de cuentas. Hacer una función que reciba esas dos cosas y devuelva la lista de cuentas con todas las transacciones ejecutadas.
5. Pero... ¡Esperen!... Necesitamos verificar que las cuentas sean estables. Con este fin, hacer una función que reciba una lista de cuentas y nos indique si todas tienen un saldo de Currycoins mayor o igual a cero.
6. **Nota:** Se puede usar recursividad en este punto.  
Hacer un chequeo de una blockchain: necesitamos una función que reciba una blockchain y una lista de cuentas, y ejecute las transacciones de los bloques sobre la lista de cuentas y nos indique si todos los estados intermedios de las cuentas (es decir, tras aplicar cada bloque) son estables.
7. Dada la siguiente función, explicar cómo se infiere su tipo:
funcionSinPudor x y 
  | (length . filter even . map (fst y) $ head x) > 10 = id
  | otherwise                                          = snd y
