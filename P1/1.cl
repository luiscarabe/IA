;;; 1.1

(defun ok-length-minus (x y)
 (if (not (equal (null (first x)) (null (first y))))
     nil
   (if (not(null (first x)))
       (if (or (minusp (first x)) (minusp (first y)))
           nil
         (ok-length-minus (rest x) (rest y)))
     t)))

;; Con recursividad

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; is-ok (x)
;;; Comprueba si una lista no es de la forma (0 0...0)
;;; INPUT: x: vector, representado como una lista
;;; OUTPUT: t si no lo es, nil si lo es

(defun is-ok (x)
  (if (null (first x))
      nil
    (if (zerop (first x))
        (is-ok (rest x))
      t)))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-pvect-rec (x y)
;;; Funcion auxiliar para sc-rec encargada de realizar el
;;; producto vectorial
;;; INPUT: x: vector, representado como una lista
;;; y: vector, representado como una lista
;;; OUTPUT: producto vectorial de x e y

(defun sc-pvect-rec (x y)
  (if (null (rest x))
  (* (first x) (first y))
  (+ (sc-pvect-rec (rest x) (rest y)) (* (first x) (first y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-rec (x y)
;;; Calcula la similitud coseno de un vector de forma recursiva
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;; La semejanza coseno entre dos vectores que son listas vacías o que son
;;; (0 0...0) es NIL.
;;; INPUT: x: vector, representado como una lista
;;; y: vector, representado como una lista
;;; OUTPUT: similitud coseno entre x e y

(defun sc-rec (x y) 
  (if (and (is-ok x) (is-ok y) (not (or (null x) (null y))))
      (/ (sc-pvect-rec x y) (* (sqrt (sc-pvect-rec x x)) (sqrt (sc-pvect-rec y y))))
    nil))


;; Con mapcar


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-pvect-mapcar (x y)
;;; Funcion auxiliar para sc-mapcar encargada de realizar el
;;; producto vectorial
;;; INPUT: x: vector, representado como una lista
;;; y: vector, representado como una lista
;;; OUTPUT: producto vectorial de x e y

(defun sc-pvect-mapcar (x y)
  (apply #'+ (mapcar #'* x y)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-mapcar (x y)
;;; Calcula la similitud coseno de un vector usando mapcar
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;; La semejanza coseno entre dos vectores que son listas vacías o que son
;;; (0 0...0) es NIL.
;;; INPUT: x: vector, representado como una lista
;;; y: vector, representado como una lista
;;; OUTPUT: similitud coseno entre x e y

(defun sc-mapcar (x y)
  (if (and (is-ok x) (is-ok y) (not (or (null x) (null y))))
      (/ (sc-pvect-mapcar x y) (* (sqrt (sc-pvect-mapcar x x)) (sqrt (sc-pvect-mapcar y y))))
    nil))

(setf l1 '(0 0 0 2))
(setf l2 '(0 0 0 0))
(setf l3 '())
(setf l4 '(2 3 4 5))

(sc-rec l1 l2)
(sc-mapcar l1 l2)
(sc-rec l1 l1)
(sc-mapcar l1 l1)
(sc-rec l1 l3)
(sc-mapcar l1 l3)
(sc-rec l1 l4)
(sc-mapcar l1 l4)
(sc-rec l4 l4)
(sc-mapcar l4 l4)



;;; 1.2 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-conf (cat vs conf)
;;; Devuelve aquellos vectores similares a una categoria
;;; INPUT: cat: vector que representa a una categoría, representado como una lista
;;; vs: vector de vectores
;;; conf: Nivel de confianza
;;; OUTPUT: Vectores cuya similitud con respecto a la categoría es superior al
;;; nivel de confianza, ordenados

(defun sc-conf (cat vs conf) 
  (if (or (null cat) (null vs) (null conf))
      nil
    (remove-if #'(lambda(x) (> conf (sc-rec x cat))) 
               (sort (copy-list vs) #'(lambda(x y) (> (sc-rec x cat) (sc-rec y cat)))))))

(setf l5 '((56 0 678) (1 2 3) (6 7 8) (1000 0 123) (10 11 12)))
(setf categ '(1 2 3))
(setf confi 0.9)

(sc-conf categ l5 confi)
l5
