(defpackage :grupo01pareja051FO0305
  (:use :common-lisp :mancala)
  (:export :heuristica :*alias*))

(in-package grupo01pareja051FO0305)

(defun heuristica (estado)
  (if (juego-terminado-p estado)
      (if (> (cuenta-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 0)
             (cuenta-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 0))
          9999 ; Si hemos ganado
        0) ; Si hemos perdido
  (valorar-Heur2 '(199 150 192 46 14 6) 0 (estado-tablero estado) (estado-lado-sgte-jugador estado))))

(defvar *alias* '|aunque_te_guste_ganar_no_es_necesario_humillar|)

(defun valorar-Heur2 (factores index tablero lado)
  (if (or (eql index 6) (null factores))
      0
    (+ (* (first factores)
          (get-fichas tablero lado index))
       (valorar-Heur2 (rest factores) (+ index 1) tablero lado))))
