(defpackage :grupo01pareja052FO2504 
  (:use :common-lisp :mancala)      
  (:export :heuristica :*alias*))

(in-package grupo01pareja052FO2504)

(defun heuristica (estado)
  (valorar-Heur estado 63 194 175))

(defvar *alias* '|Sirope_Milka|)


(defun valorar-Heur (estado factorFichas factorVacios preferenciaVacios)
  (if (juego-terminado-p estado)
      (if (> (cuenta-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 0)
             (cuenta-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 0))
          99999 ; Si hemos ganado
        0) ; Si hemos perdido    
    (+ (* factorFichas 
          (valorar-fichas estado))
       (* factorVacios 
(valorar-vacios (reverse (list-lado estado (estado-lado-sgte-jugador estado))) 0 preferenciaVacios)))))

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

