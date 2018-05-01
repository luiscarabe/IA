(defpackage :grupo01pareja053FO3004 
  (:use :common-lisp :mancala)      
  (:export :heuristica :*alias*))

(in-package grupo01pareja053FO3004)

(defun heuristica (estado)
  (if (juego-terminado-p estado)
      (if (> (cuenta-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 0)
             (cuenta-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 0))
          9999 ; Si hemos ganado
        0) ; Si hemos perdido
  (valorar-Heur2 '(88 53 65 23 9 2) 0 (estado-tablero estado) (estado-lado-sgte-jugador estado))))

(defvar *alias* '|que_bueno_que_esten_los_medios_de_comunicacion|)

(defun valorar-Heur2 (factores index tablero lado)
  (if (or (eql index 6) (null factores))
      0
    (+ (* (first factores)
          (get-fichas tablero lado index))
       (valorar-Heur2 (rest factores) (+ index 1) tablero lado))))
