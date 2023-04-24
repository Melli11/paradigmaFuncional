# ParcialPowerRangers

Es el año 1994, y la ciudad de Tokio1 el estado de California se ve amenazado por brujas y monstruos que crecen de tamaño. La solución al problema es obvia: necesitamos de un escuadrón de jóvenes multicolores que comanden robots gigantes.

![1366_2000](https://user-images.githubusercontent.com/54424951/167341493-4d59472c-97b9-4002-a79d-8f066b1ed3bd.jpg)


### Es el año 1994, y la ciudad de Tokio[1] el estado de California se ve amenazado por brujas y monstruos que crecen de tamaño. La solución al problema es obvia: necesitamos de un escuadrón de jóvenes multicolores que comanden robots gigantes. 

**1.a** Tenemos personas, de las que se conocen sus habilidades (strings) y si son buenas o no. Dar un sinónimo de tipo y una constante de ejemplo.

**1.b** Tenemos tambien Power Rangers, de los que conocemos su color, sus habilidades y su nivel de pelea. Nuevamente, dar un sinónimo de tipo y una constante de ejemplo.

**2. convertirEnPowerRanger** dado un color y una persona, convierte a la persona en un *ranger* de ese color.

Al hacer esto, le traspasa todas sus habilidades originales, potenciadas (por ejemplo: si su habilidad era “bailar”, ahora es ”superBailar”[2]), y le da tanto nivel de pelea como la cantidad de letras de todas sus habilidades originales

**3. formarEquipoRanger**: dada una lista de colores y una lista de personas, genera un equipo (una lista de Power Rangers) de los colores dados.

El equipo *ranger* está conformado por las personas buenas, transformadas en *ranger*, una para cada color que haya. Por ejemplo, asumiendo que bulk y skull son personas malas, la siguiente consulta:

**formarEquipoRanger ["rojo", "rosa", "azul"] [jason, skull, kimberly, bulk]**

Genera a un equipo conjason convertido en *ranger* rojo, ykimberlyconvertida en *ranger* rosa. Nadie se convierte en *ranger* azul.

**4.a findOrElse**: Dada una condición, un valor y una lista, devuelve el primer elemento que cumpla la condición, o el valor dado si ninguno la cumple.

**4.b rangerLider**: Dado un equipo de *rangers*, devuelve aquel que debe liderar el equipo: este es siempre el ranger rojo, o en su defecto, la cabeza del equipo.

**5.a maximumBy**: dada una lista, y una función que tome un elemento y devuelva un valor ordenable, encuentra el máximo de la misma.

**5.b rangerMásPoderoso**: devuelve el *ranger* de un equipo dado que tiene mayor nivel de pelea.

**6 rangerHabilidoso**: nos dice si un *ranger* tiene más de 5 habilidades

**7** Alfa 5 es un robot que si bien no sabe pelear, podemos considerarlo como un ranger honorífico. Su color es metálico, su habilidad de pelea es cero, y tiene dos habilidades: reparar cosas y decir “ay ay ay ay ay ay… (Sí, infinitos “ay” ) 

**7.a** Escribir la constante alfa5 

**7.b** Usando alfa5, si fuera posible, dar un ejemplo de aplicacion de una función de las definidas anteriormente que termine y otra que no.

**8** Existe otro escuadrón que combate contra el mal: las chicas superpoderosas, de las que sabemos su color y cantidad de pelo. 

Al igual que los Power Rangers, su lider es la roja, o la cabeza del equipo sin repetir lógica respecto del código ya desarrollado.

Si fuera necesario modificar funciones ya desarrolladas, escribir sus nuevas versiones.

Notas:

1. Explicitar el tipo de todas las funciones principales
1. No usar recursividad más de una vez
1. Maximizar el uso de aplicación parcial, composición y orden superior.

[1]: Los Power Rangers no son más que la adaptación de Super Sentai, clásica serie Japonesa.

[2]: Existe la función toUppercase :: Char ­> Char, que dada una letra devuelve la mayúscula de esa letra.