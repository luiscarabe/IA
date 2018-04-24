(defun f-eval-Heur (estado)
  (valorar-Heur estado 20 30 50))


;factorFichas - valor que se le da a tener mas fichas en tu lado
;factorVacios - valor que se le da a tener mas huecos vacios en tu lado
;preferenciaVacios - cuanto mas alto, mas preferencia a tener huecos en el lado mas lejano a nuestro kalaha

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

  
(defparameter *jdr-pesimillo* (make-jugador
                      :nombre '|catapumba|
                      :f-juego #'f-j-nmx
                         :f-eval #'f-eval-Heur))
