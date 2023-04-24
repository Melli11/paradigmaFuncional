Pinky y Cerebro![](Aspose.Words.ca3d445a-69d5-409d-99e0-e10d8847b9e9.001.png)

-Cerebro, ¿qué vamos a hacer esta noche? -Lo mismo que hacemos todas las noches, Pinky. ¡Tratar de conquistar el mundo!

En el laboratorio de la compañía ACME, científicos modifican genéticamente a animales para darles capacidades especiales. De estos experimentos surgieron, entre otros, Pinky y Cerebro, uno es un genio, y el otro no es tan cuerdo.

Para ayudar a los científicos en esta compleja tarea, nos han pedido que simulemos sus experimentos, resolviendo los siguientes problemas y **explicitando el tipo** de todas las funciones principales:

1. Modelar a los animales: escribir un sinónimo de tipo y definir algunos ejemplos de animales como constantes. De un animal se sabe su coeficiente intelectual (un número), su especie (un string) y sus capacidades (strings).
1. Transformar a un animal de laboratorio:
- inteligenciaSuperior: Incrementa en n unidades su coeficiente intelectual
- pinkificar: quitarle todas las habilidades que tenía
- superpoderes:  le da habilidades nuevas
- En caso de ser un elefante: le da la habilidad “no tenerle miedo a los ratones”
- En caso de ser un ratón con coeficiente intelectual mayor a 100: le agrega la habilidad de “hablar”.
- Si no, lo deja como está.
3. Los científicos muchas veces desean saber si un animal cumple ciertas propiedades, porque luego las usan como criterio de éxito de una transformación. Desarrollar los siguientes criterios:
- antropomórfico: si tiene la habilidad de hablar y su coeficiente es mayor a 60.
- noTanCuerdo: si tiene más de dos habilidades de hacer sonidos pinkiescos. Hacer una funciónpinkiesco, que significa que la habilidad empieza con “hacer ”, y luego va seguido de una palabra "pinkiesca", es decir, con 4 letras o menos y al menos una vocal. Ejemplo:
- pinkiesco “hacer narf” True
- pinkiesco “hacer asdf” True
4. Los científicos construyen experimentos: un experimento se compone de un conjunto de transformaciones sobre un animal, y un criterio de éxito. Se pide:
- Modelar a los experimentos: dar un sinónimo de tipo.
- Desarollar experimentoExitoso: Dado un experimento y un animal, indica si al aplicar sucesivamente todas las transformaciones se cumple el criterio de éxito.
- Dar un ejemplo de consulta para representar la siguiente situación:

*"En un ratón de coeficiente intelectual 17, con habilidades de destruenglonir el mundo y hacer planes desalmados, hacer un experimento que consista en pinkificarlo, luego darle inteligencia superior de 10 y por último darle superpoderes. Como criterio de éxito, ver si quedó antropomórfico"*

5. Periódicamente, ACME pide informes sobre los experimentos realizados. Desarrollar los siguientes reportes, que a partir de una lista de animales, una lista de capacidades y un experimento (o una serie de transformaciones) permitan obtener:
1. una lista con los coeficientes intelectuales de los animales que entre sus capacidades, luego de efectuar el experimento, tengan alguna de las capacidades dadas.
1. una lista con las especie de los animales que, luego de efectuar el experimento, tengan entre sus capacidades todas las capacidades dadas.
1. una lista con la cantidad de capacidades de todos los animales que, luego de efectuar el experimento, no tengan ninguna de las capacidades dadas.

*Notar que todos los reportes son parecidos. No repetir lógica. Justificar conceptualmente.*

6. Aparece un nuevo animal que tiene infinitas capacidades. Dar ejemplos de experimentos que se puedan realizar y que no, si los hubiera. Justificar conceptualmente.
6. Generar todas las posibles palabras pinkiescas. Pistas:

generateWordsUpTo, que toma una longitud y genera una lista con todas las posibles palabras de hasta la longitud dada.

generateWords que toma una longitud y genera una lista de palabras donde todas tienen exactamente la longitud dada.
