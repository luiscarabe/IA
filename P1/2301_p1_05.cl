;; Nuria Cuaresma, Luis Carabe
;; Pareja 05, grupo 2301

;;;;;;;;;;;;;;;;;
;; Ejercicio 1 ;;
;;;;;;;;;;;;;;;;;

;;; 1.1

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
;;; producto vectorial de manera recursiva
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

;;; 1.2 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-sort (cat vs fun)
;;; Ordena una lista según el grado de similitud
;;; INPUT: cat: vector que representa a una categoría, representado como una lista
;;; vs: vector de vectores
;;; fun: funcion con la que vamos a comparar
;;; OUTPUT: vectores ordenados segun la similitud con cat

(defun sc-sort (cat vs fun)
  (sort (copy-list vs) #'(lambda(x y) (> (funcall fun x cat) (funcall fun y cat)))))
  

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
      nil ;; Eliminamos los que no cumplen el grado de confianza
    (remove-if #'(lambda(x) (> conf (sc-rec x cat))) (sc-sort cat vs #'sc-rec))))

;;; 1.3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-special-sort (cats text func)
;;; Variante de sc-sort (maneja listas con un id como primer elemento)
;;; INPUT: cats: vector que representa distintas categorias, representado como una lista de listas
;;; text: lista con la que comparar
;;; func: funcion con la que vamos a comparar
;;; OUTPUT: vectores ordenados segun la similitud cats-text

(defun sc-special-sort (cats text func)
  (sort (copy-list cats) #'(lambda(x y)
                             (> (funcall func (rest x) (rest text)) (funcall func (rest y) (rest text))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-classifier-aux (cats text func)
;;; Funcion auxiliar para sc-classifier que compara un vector con
;;; todo el array de categorias y devuelve el id de la categoria
;;; mas afin junto con esa afinidad
;;; INPUT: cats: vector que representa distintas categorias, representado como una lista de listas
;;; text: lista con la que comparar
;;; func: funcion con la que vamos a comparar
;;; OUTPUT: lista de la forma (id distancia)

(defun sc-classifier-aux (cats text func)
  (let ((primer (first (sc-special-sort cats text func))))
    (list (first primer) (funcall func (rest text) (rest primer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-classifier-rec (cats texts func)
;; Clasifica a los textos en categorías de manera recursiva
;;;
;;; INPUT: cats: vector de vectores, representado como una lista de listas
;;; texts: vector de vectores, representado como una lista de listas
;;; func: función para evaluar la similitud coseno
;;; OUTPUT: Pares identificador de categoría con resultado de similitud coseno

(defun sc-classifier-rec (cats texts func)
  (if (null (first texts))
      nil
    (cons (sc-classifier-aux cats (first texts) func) (sc-classifier-rec cats (rest texts) func))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-classifier (cats texts func)
;;; Clasifica a los textos en categorías.
;;;
;;; INPUT: cats: vector de vectores, representado como una lista de listas
;;; texts: vector de vectores, representado como una lista de listas
;;; func: función para evaluar la similitud coseno
;;; OUTPUT: Pares identificador de categoría con resultado de similitud coseno
;;;

(defun sc-classifier (cats texts func)
  (if (or (null cats) (null texts) (null func))
      nil
    (sc-classifier-rec cats texts func)))


;;; 1.4 

;; Este ejercicio se refleja unicamente en la memoria



;;;;;;;;;;;;;;;;;
;; Ejercicio 2 ;;
;;;;;;;;;;;;;;;;;

;;; 2.1

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

;;; 2.2

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

;;; 2.3

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


;;;;;;;;;;;;;;;;;
;; Ejercicio 3 ;;
;;;;;;;;;;;;;;;;;

;;; 3.1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-elt-lst (elt lst)
;;; Funcion que combina un elemento dado con todos los elementos de una
;;; lista
;;; INPUT: elt elemento
;;;        lst vector, representado como una lista
;;; OUTPUT: lista con el elemento combinado

(defun combine-elt-lst (elt lst)
  (if (or (null lst))
      nil
    (mapcar #'(lambda (x) (list elt x)) lst)))

;;; 3.2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-lst-lst-rec (lst1 lst2)
;;; Funcion recursiva que realiza el producto cartesiano entre dos listas
;;; INPUT: lst1 primera lista
;;;        lst2 segunda lista
;;; OUTPUT: lista con el producto cartesiano

(defun combine-lst-lst-rec (lst1 lst2)
  (if (null (first lst1))
      nil
    (append (combine-elt-lst (first lst1) lst2) (combine-lst-lst-rec (rest lst1) lst2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-lst-lst (lst1 lst2)
;;; Funcion que realiza el producto cartesiano entre dos listas
;;; (llamando a combine-lst-lst-rec)
;;; INPUT: lst1 primera lista
;;;        lst2 segunda lista
;;; OUTPUT: lista con el producto cartesiano

(defun combine-lst-lst (lst1 lst2)
  ;; Comprobamos que no sean nil y llamamos a la funcion recursiva
  (if (or (null lst1) (null lst2))
      nil 
    (combine-lst-lst-rec lst1 lst2)))
    

;;; 3.3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flatten (lst)
;;; Funcion dada en las transparencias para pasar de una estructura
;;; de arbol a una de lista (quitando parentesis)
;;; INPUT: lst lista
;;; OUTPUT: lista con todos los elementos al mismo nivel

(defun flatten (lst) 
  (cond 
   ((null lst) NIL)
   ((atom (first lst)) 
    (cons 
     (first lst)
     (flatten (rest lst))))
   (t (append 
       (flatten (first lst))
       (flatten (rest lst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-lst-lst-spe (elt lst)
;;; Funcion que combina un elemento dado con todos los elementos de una
;;; lista, el elemento resulta ser una lista, es decir, realiza el
;;; producto cartesiano de un elemento de la forma (A B) con una lista
;;; INPUT: elt elemento a combinar (es una lista)
;;;        lst vector, representado como una lista
;;; OUTPUT: lista con el elemento combinado

(defun combine-lst-lst-spe (elt lst)
  (if (or (null lst))
      nil
    (mapcar #'(lambda (x) (flatten (list elt x))) lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-list-of-lsts-aux (lists l1)
;;; Funcion que realiza el producto cartesiano de la lista l1 con
;;; cada una de las sublistas del vector lists
;;; INPUT: lists vector de listas a las cuales se quiere hacer el 
;;;              producto cartesiano
;;;        l1 lista con la que se hace el producto cartesiano
;;; OUTPUT: lista con los productos cartesianos

(defun combine-list-of-lsts-aux (lists l1)
  (if (null (rest lists))
      (combine-lst-lst-spe (first lists) l1)
    (append (combine-lst-lst-spe (first lists) l1) (combine-list-of-lsts-aux (rest lists) l1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-list-of-lsts-rec (lstolsts)
;;; Funcion recursiva que calcula todas las posibles disposiciones de elementos
;;; pertenecientes a N listas de forma que en cada disposicion aparezca 
;;; unicamente un elemento de cada lista.
;;; INPUT: lstolsts vector de listas
;;; OUTPUT: lista con todas las disposiciones

(defun combine-list-of-lsts-rec (lstolsts)
  (if (null (rest (rest lstolsts)))
      (combine-list-of-lsts-aux (first lstolsts) (first (rest lstolsts)))
    (combine-list-of-lsts-rec 
     (cons (combine-list-of-lsts-aux (first lstolsts) (first (rest lstolsts))) (rest (rest lstolsts))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-list-of-lsts (lstolsts)
;;; Funcion que calcula todas las posibles disposiciones de elementos
;;; pertenecientes a N listas de forma que en cada disposicion aparezca 
;;; unicamente un elemento de cada lista. (Llama a combine-list-of-lsts-rec)
;;; INPUT: lstolsts vector de listas
;;; OUTPUT: lista con todas las disposiciones

       
(defun combine-list-of-lsts (lstolsts)
  (if (null lstolsts)
      (list nil)
    (if (some #'null lstolsts)
        nil
      (if (null (rest lstolsts)) ;; Cubrimos el caso en el cual solo hay una lista
          (mapcar #'(lambda (x) (list x)) (first lstolsts))
        (combine-list-of-lsts-rec lstolsts))))) ;; Caso general
