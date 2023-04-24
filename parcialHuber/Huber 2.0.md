**Huber 2.0**

![](Aspose.Words.f169900e-c223-4f64-be7f-763bf9ea775d.001.jpeg)

Una agencia de remises contrata los más eficientes choferes de los que conoce:

- el nombre
- el kilometraje de su auto
- los viajes que tomó
- qué condición impone para tomar un viaje

Cada viaje se hace en una fecha particular, lo toma un cliente (queremos saber su nombre y dónde vive) y tiene un costo.

En cuanto a la condición para tomar un viaje

- algunos choferes toman cualquier viaje
- otros solo toman los viajes que salgan más de $ 200
- otros toman aquellos en los que el nombre del cliente tenga más de n letras
- y por último algunos requieren que el cliente no viva en una zona determinada

Se pide

1. (2 puntos) Modelar los TAD cliente, chofer y viaje.
1. (2 puntos) Implementar con las abstracciones que crea conveniente las condiciones que cada chofer tiene para tomar un viaje. Debe utilizar en este punto composición y aplicación parcial.
1. (1 punto) Definir las siguientes expresiones:
1. el cliente “Lucas” que vive en Victoria
1. el chofer “Daniel”, su auto tiene 23.500 kms., hizo un viaje con el cliente Lucas el 20/04/2017 cuyo costo fue $ 150, y toma los viajes donde el cliente no viva en “Olivos”.
1. la chofer “Alejandra”, su auto tiene 180.000 kms, no hizo viajes y toma cualquier viaje.
4. (1 punto) Saber si un chofer puede tomar un viaje.
5. (2 puntos) Saber la liquidación de un chofer, que consiste en sumar los costos de cada uno de los viajes. Por ejemplo, Alejandra tiene $ 0 y Daniel tiene $ 150.
5. (4 puntos) Realizar un viaje: dado un viaje y una lista de choferes, se pide que
1. filtre los choferes que toman ese viaje. Si ningún chofer está interesado, no se preocupen: el viaje no se puede realizar.
1. considerar el chofer que menos viaje tenga. Si hay más de un chofer elegir cualquiera.
1. efectuar el viaje: esto debe incorporar el viaje a la lista de viajes del chofer. ¿Cómo logra representar este cambio de estado?
7. (1 punto) Al infinito y más allá
1. Modelar al chofer “Nito Infy”, su auto tiene 70.000 kms., que el 11/03/2017 hizo infinitos viajes de $ 50 con Lucas y toma cualquier viaje donde el cliente tenga al menos 3 letras. Puede ayudarse con esta función:

**repetirViaje** viaje = viaje : repetirViaje viaje

2. ¿Puede calcular la liquidación de Nito? Justifique.
2. ¿Y saber si Nito puede tomar un viaje de Lucas de $ 500 el 2/5/2017? Justifique.
8. (1 punto) Inferir el tipo de la función gōngnéng

**gongNeng** arg1 arg2 arg3 **=**

max arg1 . head . filter arg2 . map arg3

**Restricciones**

2 de 2

Codificar la solución utilizando t

- Modelado
- Composición
- Aplicación Parcial
- Evaluación diferida

odos estos conceptos que serán evaluados

- Orden superior
- Inferencia de tipos
- Operaciones con efecto vs. sin efecto

` `de 

**Notas**



|14 => 10|13, 12 => 9|11 => 8|10, 9 => 7|
| - | - | - | - |
|8 => 6|7 => Revisión|6 => 3|5..1 => 2|

` `de 
