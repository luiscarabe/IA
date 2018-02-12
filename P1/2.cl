;; Ejercicio 2.1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bisect (f a b tol)
;;; Funcion para calcular una raíz aproximada de una función f 
;;; INPUT: a: Extremo menor del intervalo
;;; b: Extremo mayor del intervalo
;;; tol: tolerancia para condición de parada
;;; f : función de la que buscamos raices
;;; OUTPUT: raiz de f

(defun bisect (f a b tol) 
	(if (>= (* (funcall f a) (funcall f b)) 0)
     nil
   (if (or (< (- b a) tol) (= (funcall f (/ (+ a b) 2)) 0))
     	(/ (+ a b) 2)
    (if(< (* (funcall f a) (funcall f (/ (+ a b) 2))) 0)
				(bisect f a (/ (+ a b) 2) tol)
			(bisect f (/ (+ a b) 2) b tol)))))

;;; Resultados obtenidos
(bisect #'(lambda(x) (sin (* 6.26 x))) 0.1 0.7 0.001) ;;---> 0.5016602
(bisect #'(lambda(x) (sin (* 6.26 x))) 0.0 0.7 0.001) ;;---> NIL
(bisect #'(lambda(x) (sin (* 6.28 x))) 1.1 1.5 0.001) ;;---> NIL
(bisect #'(lambda(x) (sin (* 6.28 x))) 1.1 2.1 0.001) ;;---> NIL

;; Ejercicio 2.2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; allroot (f lst tol)
;;; Funcion para calcular una races aproximadas de f
;;; INPUT: lst: lista de valores entre los que buscamos raices
;;; tol: tolerancia para condición de parada
;;; f : función de la que buscamos raices
;;; OUTPUT: lista con todas las raíces encontradas de f

(defun allroot (f lst tol)
	(if (not (null (rest lst)))
		(if (not (null (bisect f (first lst) (second lst) tol)))
			(append (list (bisect f (first lst) (second lst) tol))(allroot f (rest lst) tol))
			(allroot f (rest lst) tol))))

;;; Resultados obtenidos

(allroot #'(lambda(x) (sin (* 6.28 x))) '(0.25 0.75 1.25 1.75 2.25) 0.0001) ;; --> (0.50027466 1.0005188 1.5007629 2.001007)
(allroot #'(lambda(x) (sin (* 6.28 x))) '(0.25 0.9 0.75 1.25 1.75 2.25) 0.0001) ;; --> (0.5002166 1.0005188 1.5007629 2.001007)

;; Ejercicio 2.3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; power (N)
;;; Funcion para calcular potencias de 2
;;; INPUT: N: exponente al que quiero elevar 2
;;; OUTPUT: potencia de 2

(defun power (N)
	(if (= N 0) 
		1
		(* 2 (power (- N 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lst (a iter p div)
;;; Función para obtener la lista de valores entre los que 
;;; queremos calcular las raices de la funcion
;;; INPUT: a: Extremo menor del intervalo
;;; iter: Extremo mayor del intervalo
;;; tol: tolerancia para condición de parada
;;; f : función de la que buscamos raices
;;; OUTPUT: raiz de f

(defun lst(a iter max div)
	(if (/= iter max) 
		(cons (+ a (* iter div))(lst a (+ 1 iter) max div))
	  (list (+ a (* iter div)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; allind (f a b N tol)
;;; Función que divide un intervalo en 2^(N) secciones y
;;; busca en cada sección una raíz de la función f
;;; INPUT: a: Extremo menor del intervalo
;;; b: Extremo mayor del intervalo
;;; tol: tolerancia para condición de parada
;;; f : función de la que buscamos raices
;;; N: exponente para obtener el número de secciones en las 
;;; que dividir el intervalo
;;; OUTPUT: Lista con las raices encontradas

(defun allind (f a b N tol) 
	(allroot f (lst a 0 (power N) (/(- b a) (power N))) tol))


;; Resultados obtenidos
 (allind #'(lambda(x) (sin (* 6.28 x))) 0.1 2.25 2 0.0001) ;; --> (0.50027096 1.000503 1.5007349 2.0010324)
 (allind #'(lambda(x) (sin (* 6.28 x))) 0.1 2.25 1 0.0001) ;; --> NIL


