(defpackage :grupo01pareja052FO2804 
  (:use :common-lisp :mancala)      
  (:export :heuristica :*alias*))

(in-package grupo01pareja052FO2804)

(defun heuristica (estado)
  (if (juego-terminado-p estado)
      (if (> (cuenta-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 0)
             (cuenta-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 0))
          9999 ; Si hemos ganado
        0) ; Si hemos perdido
  (valorar-Heur2 '(148 137 78 43 19 7) 0 (estado-tablero estado) (estado-lado-sgte-jugador estado))))

(defvar *alias* '|Somebody_once_told_me_the_world_is_gonna_roll_me|)

(defun valorar-Heur2 (factores index tablero lado)
  (if (or (eql index 6) (null factores))
      0
    (+ (* (first factores)
          (get-fichas tablero lado index))
       (valorar-Heur2 (rest factores) (+ index 1) tablero lado))))
