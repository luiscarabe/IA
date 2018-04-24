(defpackage :grupo01pareja053FO2304 
  (:use :common-lisp :mancala)      
  (:export :heuristica :*alias*))

(in-package grupo01pareja053FO2304)

(defun heuristica (estado)
  (valorar-Heur estado 20 30 50))

(defvar *alias* 'Aymimadrelbixo)


(defun valorar-Heur (estado factorFichas factorVacios preferenciaVacios)
  (+ (* factorFichas 
        (valorar-fichas estado))
     (* factorVacios 
        (valorar-vacios (reverse (list-lado estado (estado-lado-sgte-jugador estado))) 0 preferenciaVacios))))

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

