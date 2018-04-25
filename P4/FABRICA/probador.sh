#!/bin/sh
###################################################################
##  GRACIAS JUANITO Y LUCIA
## Como ejecutar esta movaida:
##  ./CreateAndExecute.sh 1 2 3
## donde los 3 argumentos son las ponderaciones.
## El resultado es un unico numero que es la diferencia entre
## mi puntuacion y la del contrario, es decir, si ha salido un 4
## querra decir que he sacado 4 puntos por encima del contrario
## Si he sacado un -5, he perdido por 5 puntos
##
###################################################################



#cat mancala11.1.cl > TemporalPlayer.cl
#echo "\n\n(defvar *ponderations* '($1 $2 $3 $4 $5 $6 $7 $8 $9 $10 $11 $12))" >> TemporalPlayer.cl
replacementline=$(echo "(defvar *params* '($1 $2 $3))")
#replacement-line=$(echo hola)
sedcommand="884s/.*/${replacementline}/"
sed -i "$sedcommand" jugador.cl
#cat DefaultPlayer.cl >> TemporalPlayer.cl
#cat jugadores.cl >> TemporalPlayer.cl
#sbcl --noinform --disable-ldb --script TemporalPlayer.cl
OUTPUT=$(sbcl --noinform --disable-ldb --script jugador.cl)
echo $OUTPUT
