(defpackage :grupo01pareja052FO2404 
  (:use :common-lisp :mancala)      
  (:export :heuristica :*alias*))

(in-package grupo01pareja052FO2404)

(defun heuristica (estado)
  (valorar-Heur estado 20 90 0 80 10 100))

(defvar *alias* 'decaminoaporpollos)

(defun valorar-Heur (estado factorFichas factorVacios factorVaciosI preferenciaVacios preferenciaVaciosI facetorDiferencia)
  (+ (+ (+ (* factorFichas 
        (valorar-fichas estado))
     (* factorVacios 
        (valorar-vacios (list-lado estado (estado-lado-sgte-jugador estado)) 0 preferenciaVacios)))
     (* factorVaciosI 
        (valorar-vacios (reverse (list-lado estado (estado-lado-sgte-jugador estado))) 0 preferenciaVaciosI)))
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

