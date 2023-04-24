module Library where
import PdePreludat

nota1 = ((2,7),(6,-1))
nota2 =  ((2,2),(6,2))
nota3 = ((8,7),(-1,-1))
unAlumnoPromocionado = ((8,7),(-1,-1))
unAlumnoGustoso =   ((8,7),(6,-1))
soloUnaNota = (6,-1)
type NotaParcialUno = Number
type NotaParcialDos = Number
type NotaRecuUno = Number
type NotaRecuDos = Number
type TotalParciales = (NotaParcialUno,NotaParcialDos)
type TotalRecuperatorios = (NotaRecuUno,NotaRecuDos)
type Total_Notas = (TotalParciales,TotalRecuperatorios)

mejorNotaPrimerParcial :: Total_Notas -> Number 
mejorNotaPrimerParcial (parciales,recuperatorios)  = max (fst parciales) (fst recuperatorios) 

mejorNotaSegundoParcial :: Total_Notas -> Number
mejorNotaSegundoParcial (parciales,recuperatorios)  = max (snd parciales) (snd recuperatorios)

notasFinales :: Total_Notas -> TotalParciales
notasFinales totalParciales = (mejorNotaPrimerParcial totalParciales,mejorNotaSegundoParcial totalParciales) -- (7,8)

-- También definir la función recuperoDeGusto 
-- que dado el par de pares que representa a un alumno, nos dice si el alumno, 
-- pudiendo promocionar con los parciales (o sea sin recup.), igual rindió AL MENOS un recuperatorio.
-- si una persona no rindió un recuperatorio, entonces ponemos un "-1" en el lugar correspondiente.
recuperoDeGusto :: Total_Notas -> Bool
recuperoDeGusto unAlumno = (promociono unAlumno) && (rindioAlmenosUnRecuperatorio (notasDeRecuperatorios unAlumno)) 

promociono :: Total_Notas -> Bool
promociono unAlumno = condicionDePromocion (notasFinales unAlumno)

condicionDePromocion :: TotalParciales -> Bool
condicionDePromocion unAlumno = (notaPrimerParcial unAlumno >=7) && (notaSegundoParcial unAlumno >=7)

rindioAlmenosUnRecuperatorio :: TotalRecuperatorios -> Bool
rindioAlmenosUnRecuperatorio recu = (fst recu > 0) || (snd recu >0)

notaPrimerParcial :: TotalParciales -> Number
notaPrimerParcial unAlumno = fst  unAlumno
notaSegundoParcial :: TotalParciales -> Number
notaSegundoParcial unAlumno = snd  unAlumno

notasDeRecuperatorios :: Total_Notas -> TotalRecuperatorios
notasDeRecuperatorios unAlumno = snd unAlumno