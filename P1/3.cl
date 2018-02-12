 ;; 3.1

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

(combine-elt-lst 'a nil) ;; --> NIL
(combine-elt-lst 'a '(1 2 3)) ;; --> ((A 1) (A 2) (A 3))

;; 3.2

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
    

(combine-lst-lst nil nil) ;; --> NIL
(combine-lst-lst '(a b c) nil) ;; --> NIL
(combine-lst-lst NIL '(a b c)) ;; --> NIL
(combine-lst-lst '(a b c) '(1 2)) ;; --> ((A 1) (A 2) (B 1) (B 2) (C 1) (C 2))

;; 3.3

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
    
(combine-list-of-lsts '(() (+ -) (1 2 3 4))) ;; --> NIL
(combine-list-of-lsts '((a b c) () (1 2 3 4))) ;; --> NIL
(combine-list-of-lsts '((a b c) (1 2 3 4) ())) ;; --> NIL
(combine-list-of-lsts '((1 2 3 4))) ;; --> ((1) (2) (3) (4))
(combine-list-of-lsts '((1 2 3 4) (a b c)))
(combine-list-of-lsts '((a b c) (+ -) (1 2 3 4)))
(combine-list-of-lsts '((a b c) (+ -) (1 2) (o p)))
