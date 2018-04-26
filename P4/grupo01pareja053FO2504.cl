(defpackage :grupo01pareja053FO2504 
  (:use :common-lisp :mancala)      
  (:export :heuristica :*alias*))

(in-package grupo01pareja053FO2504)

(defun heuristica (estado)
  (valorar-Heur estado 50 90 80 100 100))

(defvar *alias* 'aversiporlomenosaprueboia)

(defun valorar-Heur (estado factorFichas factorVacios preferenciaVacios kalaha facetorDiferencia)
  (+ (+ (+ (* factorFichas 
        (valorar-fichas estado))
     (* factorVacios 
        (valorar-vacios (list-lado estado (estado-lado-sgte-jugador estado)) 0 preferenciaVacios)))
     (* kalaha 
        (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 6)))
     (* facetorDiferencia
        (- (suma-fila (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)))
           (suma-fila (estado-tablero estado) (estado-lado-sgte-jugador estado))))))


(defun valorar-fichas (estado)
  (cuenta-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 0))

(defun valorar-vacios (lista contador preferencia) ; Preferencia=0 no da preferencia
  (cond ((null lista)
         contador)
        ((eql (first lista) 0) 
         (if (eql preferencia 0)
             (valorar-vacios (rest lista) (+ contador 1) 0)
           (valorar-vacios (rest lista) (+ contador preferencia) (- preferencia 1))))
        (t
         (if (eql preferencia 0)
             (valorar-vacios (rest lista) contador 0))
           (valorar-vacios (rest lista) contador (- preferencia 1)))))


