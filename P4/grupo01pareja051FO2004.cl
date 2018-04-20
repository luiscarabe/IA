(defpackage :grupo01pareja051FO2004 
  (:use :common-lisp :mancala)      
  (:export :heuristica :*alias*))

(in-package grupo01pareja051FO2004)

(defun heuristica (estado)
  (valorar-Heur2 (reverse (list-lado estado (estado-lado-sgte-jugador estado))) 0 10))

(defvar *alias* '|miBellaCara|)

(defun valorar-Heur2 (lista contador index)
  (cond ((null lista)
         contador)
        ((eql (first lista) 0) 
         (valorar-Heur2 (rest lista) (+ contador index) (- index 1)))
        (t (valorar-Heur2 (rest lista) contador (- index 1)))))

