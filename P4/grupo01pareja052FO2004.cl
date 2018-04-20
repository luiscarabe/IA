(defpackage :grupo01pareja052FO2004 
  (:use :common-lisp :mancala)      
  (:export :heuristica :*alias*))

(in-package grupo01pareja052FO2004)

(defun heuristica (estado)
  (valorar-Heur2 (list-lado estado (estado-lado-sgte-jugador estado)) 0 15))

(defvar *alias* '|A_veces_desespero_cuando_menamoro|)

(defun valorar-Heur2 (lista contador index)
  (cond ((null lista)
         contador)
        ((eql (first lista) 0) 
         (valorar-Heur2 (rest lista) (+ contador (* 1.5 index)) (- index 1)))
        (t (valorar-Heur2 (rest lista) contador (- index 1)))))

