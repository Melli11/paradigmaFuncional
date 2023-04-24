<<<<<<< HEAD
﻿
=======
﻿Fairly odd parents

*“Timmy is an average kid that no one understands...” ![](Aspose.Words.df28e612-e891-4b73-ae61-b31e4ab6141d.001.png)*

![anigif_sub-buzz-30689-1530610991-1](https://user-images.githubusercontent.com/54424951/167044792-2e3a123e-a473-4cef-ab30-b010a4cd48eb.gif)

En  la  ciudad  de  Dimmsdale,  los  chicos  pueden  contar  con  padrinos  mágicos  que  cumplen  sus  deseos.  Y  Jorgen  Von  Strángulo,   encargado  de  supervisar  toda  la  actividad  mágica  del  lugar,  nos  encargó  un  sistema  funcional  para  poder  entender a los ahijados. 

De los chicos se conoce su **nombre**, **edad**, **sus habilidades** y sus **deseos**. Por ejemplo: timmy = Chico “Timmy” 10 [“mirar television”, “jugar en la pc”] [serMayor]

1. **Concediendo deseos**
   1. Desarrollar los siguiente deseos y declarar el data Chico
      1. aprenderHabilidades habilidades unChico : agrega una lista de habilidades nuevas a las que ya tiene el chico.
      2. serGrosoEnNeedForSpeed unChico: dado un chico, le agrega las habilidades de  jugar  a  todas  las  versiones  pasadas  y  futuras  del  Need  For  Speed,  que son: “jugar need for speed 1”, “jugar need for speed 2”, etc.
      3. serMayor unChico: Hace que el chico tenga 18 años.

   2. Los  padrinos  son  seres  mágicos  capaces  de  cumplir  los  deseos  de  sus  ahijados.  Desarrollar los siguientes padrinos:
      1. wanda: dado un chico, wanda le cumple el primer deseo y lo hace madurar (crecer un año de edad).
      2. cosmo: dado un chico, lo hace “des”madurar, quedando con la mitad de años de edad. Como es olvidadizo, no le concede ningún deseo.
      3. muffinMagico: dado un chico le concede todos sus deseos. 

***Nota importante***: no debe haber lógica repetida entre* wanda*,* cosmo *y* serMayor

2. **En busqueda de pareja**
   1. Se acerca el baile de fin de año y se quiere saber cuáles van a ser las parejas. Para esto las chicas tienen condiciones para elegir al chico con el que van a salir, algunas de ellas son:
      1. **tieneHabilidad unaHabilidad unChico**: Dado un chico y una habilidad, dice si la posee.
      2. **esSuperMaduro**: Dado un chico dice si es mayor de edad (es decir, tiene más de 18 años) y además sabe manejar.
   
   2. Las chicas tienen un nombre, y una condición para elegir al chico con el que van ir al baile. Ejemplos:
    -- para Trixie la única condición es que el chico no sea Timmy, 
    --ya que nunca saldría con él
    trixie = Chica “Trixie Tang” noEsTimmy  
    vicky = Chica “Vicky” (tieneHabilidad “ser un supermodelo noruego”) 

    **Se pide definir el data Chica y desarrollar las siguientes funciones:**

   3. **quienConquistaA unaChica losPretendientes**: Dada una chica y una lista de pretendientes, devuelve al que se queda con la chica, es decir, el primero que cumpla con la condición que ella quiere. Si no hay ninguno que la cumpla, devuelve el último pretendiente (una chica nunca se queda sola). (Sólo en este punto se puede usar recursividad)
   
   4. Dar un ejemplo de consulta para una nueva chica, cuya condición para elegir a un chico es que este sepa cocinar.

3. **Da Rules ![](Aspose.Words.df28e612-e891-4b73-ae61-b31e4ab6141d.002.png)**

Como no todo está permitido en el mundo mágico, Jorgen  Von Strángulo está encargado de controlar que se no se  viole lo establecido en “da Rules”: 

-   **infractoresDeDaRules**:  Dada  una  lista  de  chicos,  devuelve  la  lista  de  los  nombres  de  aquellos que tienen deseos prohibidos.Un deseo  está  prohibido  si,  al  aplicarlo,  entre  las  cinco primeras habilidades, hay alguna prohibida.  En  tanto,  son  habilidades  prohibidas  enamorar,  matar y dominar el mundo. 

4. **Justificaciones**  

Indicar donde se utilizó y para qué:

- la función composición
- funciones de orden superior (creadas) 
- aplicación parcial
- listas infinitas: dar ejemplos de consultas que funcionen y que no, donde aparezcan estas listas
>>>>>>> c6dcdd39df8203af4ac47df342bb1a5c780d962c
