(defpackage :grupo01pareja051FO3004 
  (:use :common-lisp :mancala)      
  (:export :heuristica :*alias*))

(in-package grupo01pareja051FO3004)

(defun heuristica (estado)
  (if (juego-terminado-p estado)
      (if (> (cuenta-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 0)
             (cuenta-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 0))
          9999 ; Si hemos ganado
        0) ; Si hemos perdido
  (valorar-Heur2 '(88 86 66 21 5 2) 0 (estado-tablero estado) (estado-lado-sgte-jugador estado))))

(defvar *alias* '|pero_yo_quiero_a_mi_abogado|)

(defun valorar-Heur2 (factores index tablero lado)
  (if (or (eql index 6) (null factores))
      0
    (+ (* (first factores)
          (get-fichas tablero lado index))
       (valorar-Heur2 (rest factores) (+ index 1) tablero lado))))
