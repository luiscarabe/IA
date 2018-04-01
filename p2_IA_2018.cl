;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Lab assignment 2: Search
;;    LAB GROUP: 2301
;;    Couple: 05
;;    Author 1: Nuria Cuaresma Saturio
;;    Author 2: Luis Carabe Fdez-Pedraza
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Problem definition
;;
(defstruct problem
  states               ; List of states
  initial-state        ; Initial state
  f-h                  ; reference to a function that evaluates to the 
                       ; value of the heuristic of a state
  f-goal-test          ; reference to a function that determines whether 
                       ; a state fulfils the goal 
  f-search-state-equal ; reference to a predictate that determines whether
                       ; two nodes are equal, in terms of their search state      
  operators)           ; list of operators (references to functions) to 
                       ; generate successors
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Node in search tree
;;
(defstruct node 
  state           ; state label
  parent          ; parent node
  action          ; action that generated the current node from its parent
  (depth 0)       ; depth in the search tree
  (g 0)           ; cost of the path from the initial state to this node
  (h 0)           ; value of the heurstic
  (f 0))          ; g + h 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Actions 
;;
(defstruct action
  name              ; Name of the operator that generated the action
  origin            ; State on which the action is applied
  final             ; State that results from the application of the action
  cost )            ; Cost of the action
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Search strategies 
;;
(defstruct strategy
  name              ; name of the search strategy
  node-compare-p)   ; boolean comparison
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    END: Define structures
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    BEGIN: Define galaxy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *planets* '(Avalon Davion Katril Kentares Mallory Proserpina Sirtis))

(defparameter *white-holes*  
  '((Avalon Mallory 6.4) (Avalon Proserpina 8.6) (Kentares Avalon 3) (Kentares Katril 10)
    (Kentares Proserpina 7) (Mallory Katril 10) (Mallory Proserpina 15) (Proserpina Avalon 8.6)
    (Proserpina Mallory 15) (Proserpina Davion 5) (Proserpina Sirtis 12) (Katril Mallory 10) 
    (Katril Davion 9) (Davion Proserpina 5) (Davion Sirtis 6) (Sirtis Davion 6) (Sirtis Proserpina 12)))

(defparameter *worm-holes*  
  '((Avalon Kentares 4) (Avalon Mallory 9)
    (Mallory Avalon 9) (Mallory Proserpina 11) (Mallory Katril 5)
    (Proserpina Kentares 12) (Proserpina Mallory 11) (Proserpina Sirtis 9)
    (Katril Mallory 5) (Katril Davion 5) (Katril Sirtis 10)
    (Davion Katril 5) (Davion Sirtis 8)
    (Sirtis Davion 8) (Sirtis Katril 10) (Sirtis Proserpina 9) 
    (Kentares Avalon 4) (Kentares Proserpina 12)))
 
(defparameter *sensors* 
  '((Avalon 15) (Davion 5) (Mallory 12) (Kentares 14) (Proserpina 7) 
    (Katril 9) (Sirtis 0)))

(defparameter *planet-origin* 'Mallory)
(defparameter *planets-destination* '(Sirtis))
(defparameter *planets-forbidden*   '(Avalon))
(defparameter *planets-mandatory*   '(Katril Proserpina))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; BEGIN: Exercise 1 -- Evaluation of the heuristic
;;
;; Returns the value of the heuristics for a given state
;;
;;  Input:
;;    state: the current state (vis. the planet we are on)
;;    sensors: a sensor list, that is a list of pairs
;;                (state cost)
;;             where the first element is the name of a state and the second
;;             a number estimating the cost to reach the goal
;;
;;  Returns:
;;    The cost (a number) or NIL if the state is not in the sensor list
;;
(defun f-h-galaxy (state sensors)
  (second (assoc state sensors)))


(f-h-galaxy 'Sirtis *sensors*) ;-> 0
(f-h-galaxy 'Avalon *sensors*) ;-> 15
(f-h-galaxy 'Earth  *sensors*) ;-> NIL


;;
;; END: Exercise 1 -- Evaluation of the heuristic
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 2 -- Navigation operators
;;

;; navigate-white-hole
;;
;; Returns the possible moves (actions) from a given node using white-holes
;;
;;  Input:
;;    state: the current state (vis. the planet we are on)
;;    white-holes: the connections between planets using white-holes and its costs (list of lists)
;;
;;  Returns:
;;    List of actions that can be performed 
;;


(defun navigate-white-hole (state white-holes)
  ; Llamamos a navigate, con nil en planets-forbidden
  (navigate state white-holes nil 'NAVIGATE-WHITE-HOLE))

;; navigate-worm-hole
;;
;; Returns the possible moves (actions) from a given node using worm-holes
;;
;;  Input:
;;    state: the current state (vis. the planet we are on)
;;    worm-holes: the connections between planets using worm-holes and its costs (list of lists)
;;    planets-forbidden: list of planets that we must avoid
;;
;;  Returns:
;;    List of actions that can be performed 
;;

(defun navigate-worm-hole (state worm-holes planets-forbidden)
  ; Llamamos a navigate
  (navigate state worm-holes planets-forbidden 'NAVIGATE-WORM-HOLE))



; Funcion generica para simular la navegacion entre los planetas, dependiendo de la lista de agujeros espaciales que entre por argumento

(defun navigate (state holes planets-forbidden opname)
  
  ; Eliminamos elementos nulos en la lista de acciones
  
  (remove nil (mapcar #'(lambda(conexion) 
                          (unless (member (second conexion) planets-forbidden) ; Comprobamos que no visitamos planeta prohibido
                            ; Creamos la accion rellenando los parametros correspondientes
                            (setq action (make-action :name opname
                                                      :origin state
                                                      :final (second conexion)
                                                      :cost (third conexion)))))
                ; Creamos una lista con las conexiones que tiene el planeta en el que estamos
                ; segun la lista de agujeros blancos/de gusano que nos hayan pasado
                (remove-if-not (lambda(ini-planet) (eq (car ini-planet) state)) holes))))


(navigate-worm-hole 'Mallory *worm-holes* *planets-forbidden*)  ;-> 
;;;(#S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL KATRIL :COST 5)
;;; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL PROSERPINA :COST 11))

(navigate-worm-hole 'Mallory *worm-holes* NIL)  ;-> 
;;;(#S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL AVALON :COST 9)
;;; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL KATRIL :COST 5)
;;; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL PROSERPINA :COST 11))


(navigate-white-hole 'Kentares *white-holes*) ;->
;;;(#S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES :FINAL AVALON :COST 3)
;;; #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES :FINAL KATRIL :COST 10)
;;; #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES :FINAL PROSERPINA :COST 7))


(navigate-worm-hole 'Uranus *worm-holes* *planets-forbidden*)  ;-> NIL


;;
;; END: Exercise 2 -- Navigation operators
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 3 -- Goal test
;;
;;
;; Test if we have reached the goal
;;
;;  Input:
;;    node: the current node (vis. the planet we are on)
;;    planets-destination: the list of possible destination planets 
;;    planets-mandatory: list of planets that we must have visited
;;
;;  Returns:
;;    T if the node satisfies the goal test, nil in other case
;;

(defun f-goal-test-galaxy (node planets-destination planets-mandatory) 
  ;; T si estamos en un planeta de destino y hemos visitado todos los planetas obligatorios
  (and (member (node-state node) planets-destination) 
       (visited-all-mandatory-planets (create-list-of-parents node) planets-mandatory)))


;; Funcion que nos dice si hemos visitado todos los planetas obligatorios
;; Recibe: lista de planetas visitados y lista de planetas que debemos visitar

(defun visited-all-mandatory-planets (planets-visited planets-mandatory)
  (if (null planets-mandatory)
      t ;; Si los planetas obligatorios se han acabado, devolvemos true
    ;; Comprobamos que el primer planeta obligatorio este en los visitados
    (and (member (first planets-mandatory) planets-visited)
         ;; y que el resto de los planetas obligatorios de la lista esten tambien
         (visited-all-mandatory-planets planets-visited (rest planets-mandatory)))))


;; Funcion que crea la lista de padres de un nodo, incluyendole

(defun create-list-of-parents (node)
  (if (null (node-parent node))
      ;; Si el nodo ya no tiene padre, le incluimos en la lista
      (list (node-state node))
    ;; Si tiene padre, incluimos al nodo en la lista y llamamos a la funcion pasandole a su padre
    (cons (node-state node) (create-list-of-parents(node-parent node)))))


(defparameter node-01
   (make-node :state 'Avalon) )
(defparameter node-02
   (make-node :state 'Kentares :parent node-01))
(defparameter node-03
   (make-node :state 'Katril :parent node-02))
(defparameter node-04
   (make-node :state 'Kentares :parent node-03))
(f-goal-test-galaxy node-01 '(kentares urano) '(Avalon Katril)); -> NIL
(f-goal-test-galaxy node-02 '(kentares urano) '(Avalon Katril)); -> NIL
(f-goal-test-galaxy node-03 '(kentares urano) '(Avalon Katril)); -> NIL
(f-goal-test-galaxy node-04 '(kentares urano) '(Avalon Katril)); -> T


;;
;; END: Exercise 3 -- Goal test
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise  -- Equal predicate for search states
;;
;; Test if two nodes are the same
;;
;;  Input:
;;    node-1: first node
;;    node-2: second node
;;    planets-mandatory: list of planets that we must have visited
;;
;;  Returns:
;;    T if the nodes are the same, nil in other case
;;

(defun f-search-state-equal-galaxy (node-1 node-2 &optional planets-mandatory)
  ;; Son iguales si:
  (and (eql (node-state node-1) (node-state node-2)) ;; Corresponden al mismo planeta
       (same-mandatory-to-visit node-1 node-2 planets-mandatory))) ;; Les falta por visitar los mismos planetas obligatorios


;; Funcion que comprueba si a dos nodos les faltan los mismos planetas obligatorios por visitar

(defun same-mandatory-to-visit (node-1 node-2 planets-mandatory)
  ;; Comprobamos que las dos siguientes listas sean iguales:
  (same-list 
   ;; Los planetas obligatorios que no estan entre los padres del nodo 1
   (set-difference planets-mandatory (create-list-of-parents node-1))
   ;; Los planetas obligatorios que no estan entre los padres del nodo 2
   (set-difference planets-mandatory (create-list-of-parents node-2))))


;; Funcion que comprueba si dos listas son iguales

(defun same-list (list1 list2)
  ;; Son iguales si:
  (and (null (set-difference list1 list2)) ;; Todos los elementos de list1 estan en list2
       (null (set-difference list2 list1)))) ;; Todos los elementos de list2 estan en list1
     
       
(f-search-state-equal-galaxy node-01 node-01) ;-> T
(f-search-state-equal-galaxy node-01 node-02) ;-> NIL
(f-search-state-equal-galaxy node-02 node-04) ;-> T

(f-search-state-equal-galaxy node-01 node-01 '(Avalon)) ;-> T
(f-search-state-equal-galaxy node-01 node-02 '(Avalon)) ;-> NIL
(f-search-state-equal-galaxy node-02 node-04 '(Avalon)) ;-> T

(f-search-state-equal-galaxy node-01 node-01 '(Avalon Katril)) ;-> T
(f-search-state-equal-galaxy node-01 node-02 '(Avalon Katril)) ;-> NIL
(f-search-state-equal-galaxy node-02 node-04 '(Avalon Katril)) ;-> NIL


;;
;; END: Exercise  -- Equal predicate for search states
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BEGIN: Exercise 4 -- Define the galaxy structure
;;
;;

(defparameter *galaxy-M35* 
  (make-problem 
   :states               *planets*          
   :initial-state        *planet-origin*
   :f-h                  #'(lambda (state) (f-h-galaxy state *sensors*)) ;; Funcion que nos da el valor h de un estado
   :f-goal-test          #'(lambda (node) (f-goal-test-galaxy node *planets-destination* *planets-mandatory*)) ;; Funcion que nos dice si estamos en el objetivo
   :f-search-state-equal #'(lambda (node-1 node-2) (f-search-state-equal-galaxy node-1 node-2 *planets-mandatory*)) ;; Funcion que nos dice si dos nodos son iguales
   :operators            (list ;; Funciones para navegar por agujeros blancos y de gusano
                          #'(lambda (node)
                              (navigate-white-hole (node-state node) *white-holes*))
                          #'(lambda (node)
                              (navigate-worm-hole (node-state node) *worm-holes* *planets-forbidden*)))))


;;
;;  END: Exercise 4 -- Define the galaxy structure
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN Exercise 5: Expand node
;;
;; Expands one given node
;;
;;  Input:
;;    node: the node to expand
;;    problem: the struct with the current problem
;;
;;  Returns:
;;    The list of generated nodes
;;

(defun expand-node (node problem)
  ;; Para cada operador de la lista de operadores del problema, llama a la funcion que se encarga de expandir el nodo segun el operador
  (mapcan #'(lambda(operator) (expand-node-aux node operator problem)) (problem-operators problem)))


;; Funcion que, dado un operador, el nodo origen y el problema, crea la lista de todos los nodos resultantes de usar dicho operador sobre dicho nodo origen

(defun expand-node-aux (node operator problem)
  ;; Creamos el nodo correspondiente a aplicar cada accion
  (mapcar #'(lambda(accion)
              ;; Guardamos el valor g y h
              (let 
                  ;; g (coste acumulado) resulta de la suma del anterior coste acumulado mas el coste de pasar de un nodo a otro
                  ((g (+ (node-g node) (action-cost accion))) 
                   ;; Sacamos h gracias a la funcion guardada en la estructura del problema 
                   (h (funcall (problem-f-h problem) (action-final accion))))
                ;; Creamos el nodo y rellenamos sus parametros
                (make-node :state (action-final accion) ;; Estado final de la accion
                           :parent node
                           :action accion
                           :depth (1+ (node-depth node)) ;; Incrementamos la profundidad del nodo padre
                           :g g
                           :h h
                           :f (+ g h))))
    ;; Conseguimos la lista de acciones segun el operador
    (funcall operator node)))

(defparameter node-00
   (make-node :state 'Proserpina :depth 12 :g 10 :f 20) )

(defparameter lst-nodes-00
  (expand-node node-00 *galaxy-M35*)) 


(print lst-nodes-00)

;;;(#S(NODE :STATE AVALON
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL AVALON :COST 8.6)
;;;         :DEPTH 13   :G 18.6  :H 15  :F 33.6)
;;; #S(NODE :STATE DAVION
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL DAVION :COST 5)
;;;         :DEPTH 13   :G 15    :H 5   :F 20)
;;; #S(NODE :STATE MALLORY
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 15)
;;;         :DEPTH 13   :G 25    :H 12  :F 37)
;;; #S(NODE :STATE SIRTIS
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 12)
;;;         :DEPTH 13   :G 22    :H 0   :F 22)
;;; #S(NODE :STATE KENTARES
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL KENTARES :COST 12)
;;;         :DEPTH 13   :G 22    :H 14  :F 36)
;;; #S(NODE :STATE MALLORY
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 11)
;;;         :DEPTH 13   :G 21    :H 12  :F 33)
;;; #S(NODE :STATE SIRTIS
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 9)
;;;         :DEPTH 13   :G 19    :H 0   :F 19))



;;
;; END Exercise 5: Expand node
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  BEGIN Exercise 6 -- Node list management
;;;  
;Definimos la funcion node-g
(defun node-g-<= (node-1 node-2)
	(<= (node-g node-1)
     (node-g node-2)))

;Definimos la estrategia unidorm-cost
(defparameter *uniform-cost*
  (make-strategy
   :name 'uniform-cost
   :node-compare-p #'node-g-<=))

; Inserta de manera ordenada, segun la estrategia, un nodo en la lista
(defun insert-node (nodes lst-nodes strategy)
  (if (funcall (strategy-node-compare-p strategy) (first nodes) (first lst-nodes)); comprobamos si el nodo va antes que el primero de la lista
      (insert-nodes-strategy (rest nodes) (cons (first nodes) lst-nodes) strategy) ; en ese caso lo insertamos y volvemos a llamar a la otra de manera recursiva
    (cons (first lst-nodes) (insert-node nodes (rest lst-nodes) strategy)))) ;comprobamos con el resto de la lista

(defun insert-nodes-strategy (nodes lst-nodes strategy)
  (cond ((not(or nodes lst-nodes)) '())
 		  ((null strategy) nil)
 		  ((null nodes) lst-nodes) ; comprobamos que no sean null ni los nodos ni la estrategia
    	(T(insert-node nodes lst-nodes strategy))))

; (defun insert-nodes-strategy (nodes lst-nodes strategy)
;   (if (null nodes)  '()
;     (let ((nodo-a-insertar (first nodes)) 
;           (nodo-a-comparar (first lst-nodes))) 
;       (if (funcall (strategy-node-compare-p strategy) nodo-a-insertar nodo-a-comparar)
;           (insert-nodes-strategy (rest nodes) (cons nodo-a-insertar lst-nodes) strategy)
;         (cons nodo-a-comparar (insert-nodes-strategy nodes (rest lst-nodes) strategy))))))


; (defun insert-nodes-strategy (nodes lst-nodes strategy)
; 	(cond ((not(or nodes lst-nodes)) '())
; 		  ((null strategy) nil)
; 		  ((null nodes) lst-nodes)
;   		  ; si no son null ni los nodos ni la estrategia
; 		  ((funcall (strategy-node-compare-p strategy) (first nodes) (first lst-nodes)); comprobamos si el nodo va antes que el primero de la lista
; 		      (insert-nodes-strategy (rest nodes) (cons (first nodes) lst-nodes) strategy)) ; en ese caso lo insertamos
; 		  (T(cons (first lst-nodes) (insert-nodes-strategy nodes (rest lst-nodes) strategy))))) ;comprobamos con el resto de la lista


(defparameter node-01
   (make-node :state 'Avalon :depth 0 :g 0 :f 0) )
(defparameter node-02
   (make-node :state 'Kentares :depth 2 :g 50 :f 50) )


(print (insert-nodes-strategy (list node-00 node-01 node-02) 
                        lst-nodes-00 
                        *uniform-cost*));->
;;;(#S(NODE :STATE AVALON 
;;;         :PARENT NIL 
;;;         :ACTION NIL 
;;;         :DEPTH 0 :G 0 :H 0 :F 0)
;;; #S(NODE :STATE PROSERPINA 
;;;         :PARENT NIL 
;;;         :ACTION NIL 
;;;         :DEPTH 12 :G 10 :H 0 :F 20)
;;; #S(NODE :STATE AVALON
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL AVALON :COST 8.6)
;;;         :DEPTH 13    :G 18.6    :H 15    :F 33.6)
;;; #S(NODE :STATE DAVION
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL DAVION :COST 5)
;;;         :DEPTH 13    :G 15      :H 5     :F 20)
;;; #S(NODE :STATE MALLORY
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 15)
;;;         :DEPTH 13    :G 25      :H 12    :F 37)
;;; #S(NODE :STATE SIRTIS
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 12)
;;;         :DEPTH 13    :G 22      :H 0     :F 22)
;;; #S(NODE :STATE KENTARES
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL KENTARES :COST 12)
;;;         :DEPTH 13    :G 22      :H 14    :F 36)
;;; #S(NODE :STATE MALLORY
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 11)
;;;         :DEPTH 13    :G 21      :H 12    :F 33)
;;; #S(NODE :STATE SIRTIS
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 9)
;;;         :DEPTH 13    :G 19      :H 0     :F 19)
;;; #S(NODE :STATE KENTARES 
;;;         :PARENT NIL 
;;;         :ACTION NIL 
;;;         :DEPTH 2 :G 50 :H 0 :F 50)) 


(print 
 (insert-nodes-strategy (list node-00 node-01 node-02) 
                        (sort (copy-list lst-nodes-00) #'<= :key #'node-g) 
                        *uniform-cost*));->
;;;(#S(NODE :STATE AVALON 
;;;         :PARENT NIL 
;;;         :ACTION NIL 
;;;         :DEPTH 0    :G 0     :H 0   :F 0)
;;; #S(NODE :STATE PROSERPINA 
;;;         :PARENT NIL 
;;;         :ACTION NIL 
;;;         :DEPTH 12   :G 10    :H 0   :F 20)
;;; #S(NODE :STATE DAVION
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL DAVION :COST 5)
;;;         :DEPTH 13   :G 15    :H 5   :F 20)
;;; #S(NODE :STATE AVALON
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL AVALON :COST 8.6)
;;;         :DEPTH 13   :G 18.6  :H 15  :F 33.6)
;;; #S(NODE :STATE SIRTIS
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 9)
;;;         :DEPTH 13   :G 19    :H 0   :F 19)
;;; #S(NODE :STATE MALLORY
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 11)
;;;         :DEPTH 13   :G 21    :H 12  :F 33)
;;; #S(NODE :STATE KENTARES
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL KENTARES :COST 12)
;;;         :DEPTH 13   :G 22    :H 14  :F 36)
;;; #S(NODE :STATE SIRTIS
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 12)
;;;         :DEPTH 13   :G 22    :H 0   :F 22)
;;; #S(NODE :STATE MALLORY
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 15)
;;;         :DEPTH 13   :G 25    :H 12  :F 37)
;;; #S(NODE :STATE KENTARES 
;;;         :PARENT NIL 
;;;         :ACTION NIL 
;;;         :DEPTH 2    :G 50    :H 0   :F 50))


(insert-nodes-strategy '(4 8 6 2) '(1 3 5 7)
		(make-strategy 	:name 'simple
					:node-compare-p #'<));-> (1 2 3 4 5 6 7)
 


;;
;;    END: Exercize 6 -- Node list management
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 7 -- Definition of the A* strategy
;;
;; A strategy is, basically, a comparison function between nodes to tell 
;; us which nodes should be analyzed first. In the A* strategy, the first 
;; node to be analyzed is the one with the smallest value of g+h
;;

;Definimos la funcion node-f
(defun node-f-<= (node-1 node-2)
	(<= (node-f node-1)
     (node-f node-2)))

;Definimos la estrategia A-star
(defparameter *A-star*
  (make-strategy  
   :name 'A-star
   :node-compare-p #'node-f-<=))

;;
;; END: Exercise 7 -- Definition of the A* strategy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;    BEGIN Exercise 8: Search algorithm
;;;	   Realiza la búsqueda para el problema dado utilizando una estrategia
;;; ARGUMENTOS:
;;;		open-list: lista de nodos generados, pero no explorados
;;; 	closed-list: lista de nodos generados y explorados
;;; 	strategy: estrategia de búsqueda implementada como una ordenación de la lista open-nodes
;;; 	problem: problema a resolver
;;; Evalúa:
;;;		Si no hay solución: NIL
;;;		Si hay solución: un nodo que cumple el test objetivo(goal-node)
;;;

;;Funcion que realiza la recursion
(defun graph-search-rec (open-nodes closed-nodes strategy problem)
  ; extraer el primer nodo de la lista open-nodes
  (let ((fnode (first open-nodes)))
  ;si la lista open-nodes está vacía terminar[no se han encontrado solución]
  (cond ((null fnode) nil)
          ; si dicho nodo cumple el objetivo devolver y terminar.
          ((funcall(problem-f-goal-test problem) fnode) fnode)
          ; en caso contrario
          ; si el nodo no está en closed-nodes o
          ((or (not (member fnode closed-nodes)) 
               ; esta en la lista pero con coste g inferior al del que está en closed-nodes
               ((node-g-<= fnode (first (member fnode closed-nodes)))))
           ; * expandir el nodo e insertar los nodos generados en la lista open-nodes de acuerdo con la estrategia strategy.
           (graph-search-rec (insert-nodes-strategy (expand-node fnode problem) (rest open-nodes) strategy)
                             ; * incluir el nodo recién expandido al comienzo de la lista closed-nodes.
                             (cons fnode closed-nodes) strategy problem))
          ; Continuar la búsqueda eliminando el nodo considerado de la lista open-nodes.
          (T(graph-search-rec (rest open-nodes) closed-nodes strategy problem)))))


(defun graph-search (problem strategy)
  ;Inicializar la lista de nodos open-nodes con el estado inicial
  (let((open-list (list (make-node : state (problem-initial-state problem)))))
  ;llamamos a la funcion recursiva
  ;Pasamos la lista de nodos closed-nodes como lista vacía
  (graph-search-rec open-list '() strategy problem)))
;
;  Solve a problem using the A* strategy
;
(defun a-star-search (problem)
	(graph-search problem *A-star*))


(graph-search *galaxy-M35* *A-star*);->
;;;#S(NODE :STATE ...
;;;        :PARENT #S(NODE :STATE ...
;;;                        :PARENT #S(NODE :STATE ...)) 


(print (a-star-search *galaxy-M35*));->
;;;#S(NODE :STATE ...
;;;        :PARENT #S(NODE :STATE ...
;;;                        :PARENT #S(NODE :STATE ...)) 


;;; 
;;;    END Exercise 8: Search algorithm
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;    BEGIN Exercise 9: Solution path / action sequence
;;;
;;;Función que muestra el camino seguido para llegar a un nodo.
(defun solution-path (node)
	(if (null node) '()
  	(reverse (create-list-of-parents node)))) ;; mostramos los padres del nodo

(solution-path nil) ;;; -> NIL 
(solution-path (a-star-search *galaxy-M35*))  ;;;-> (MALLORY ...)

(defun action-sequence (node)
	(if (null node) '()
	(reverse(cons (node-action node) (action-sequence (node-parent))))))
		

(action-sequence (a-star-search *galaxy-M35*))
;;; ->
;;;(#S(ACTION :NAME ...)) 

;;; 
;;;    END Exercise 9: Solution path / action sequence
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;    BEGIN Exercise 10: depth-first / breadth-first
;;;

(defparameter *depth-first*
  (make-strategy
   :name 'depth-first
   :node-compare-p #'depth-first-node-compare-p))

;estrategia para realizar búsqueda en profundidad
(defun depth-first-node-compare-p (node-1 node-2)
  )

(solution-path (graph-search *galaxy-M35* *depth-first*))
;;; -> (MALLORY ... )

(defparameter *breadth-first*
  (make-strategy
   :name 'breadth-first
   :node-compare-p #'breadth-first-node-compare-p))

;estrategia para realizar búsqueda en anchura
(defun breadth-first-node-compare-p (node-1 node-2)
  )

(solution-path (graph-search *galaxy-M35* *breadth-first*))
;; -> (MALLORY ... )

;;; 
;;;    END Exercise 10: depth-first / breadth-first
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
