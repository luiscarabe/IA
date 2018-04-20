(defpackage :grupo01pareja052FO1804 
  (:use :common-lisp :mancala)      
  (:export :heuristica :*alias*))

(in-package grupo01pareja052FO1804)

(defun heuristica (estado)
  (valorar-Heur2 (list-lado estado (estado-lado-sgte-jugador estado)) 0 10))

(defvar *alias* 'TeNgOmIeDo)

(defun valorar-Heur2 (lista contador index)
  (cond ((null lista)
         contador)
        ((eql (first lista) 0) 
         (valorar-Heur2 (rest lista) (* 20 (+ contador index)) (- index 1)))
        (t (valorar-Heur2 (rest lista) contador (- index 1)))))

