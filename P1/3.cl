 ;; 3.1

(defun combine-elt-lst (elt lst)
  (if (or (null lst))
      nil
    (mapcar #'(lambda (x) (list elt x)) lst)))

(combine-elt-lst 'a nil) ;; --> NIL
(combine-elt-lst 'a '(1 2 3)) ;; --> ((A 1) (A 2) (A 3))

;; 3.2

(defun combine-lst-lst-rec (lst1 lst2)
  (if (null (first lst1))
      nil
    (append (combine-elt-lst (first lst1) lst2) (combine-lst-lst-rec (rest lst1) lst2))))

(defun combine-lst-lst (lst1 lst2)
  (if (or (null lst1) (null lst2))
      nil
    (combine-lst-lst-rec lst1 lst2)))
    

(combine-lst-lst nil nil) ;; --> NIL
(combine-lst-lst '(a b c) nil) ;; --> NIL
(combine-lst-lst NIL '(a b c)) ;; --> NIL
(combine-lst-lst '(a b c) '(1 2)) ;; --> ((A 1) (A 2) (B 1) (B 2) (C 1) (C 2))

;; 3.3

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

(defun combine-lst-lst-spe (elt lst)
  (if (or (null lst))
      nil
    (mapcar #'(lambda (x) (flatten (list elt x))) lst)))

(defun combine-list-of-lsts-aux (lists l1)
  (if (null (rest lists))
      (combine-lst-lst-spe (first lists) l1)
    (append (combine-lst-lst-spe (first lists) l1) (combine-list-of-lsts-aux (rest lists) l1))))
      

(defun combine-list-of-lsts-rec (lstolsts)
  (if (null (rest (rest lstolsts)))
      (combine-list-of-lsts-aux (first lstolsts) (first (rest lstolsts)))
    (combine-list-of-lsts-rec 
     (cons (combine-list-of-lsts-aux (first lstolsts) (first (rest lstolsts))) (rest (rest lstolsts))))))
       
       
(defun combine-list-of-lsts (lstolsts)
  (if (null lstolsts)
      (list nil)
    (if (some #'null lstolsts)
        nil
      (combine-list-of-lsts-rec lstolsts))))
    
(combine-list-of-lsts '(() (+ -) (1 2 3 4))) ;; --> NIL
(combine-list-of-lsts '((a b c) () (1 2 3 4))) ;; --> NIL
(combine-list-of-lsts '((a b c) (1 2 3 4) ())) ;; --> NIL
(combine-list-of-lsts '((1 2 3 4))) ;; --> ((1) (2) (3) (4))
(combine-list-of-lsts '((1 2 3 4) (a b c)))
(combine-list-of-lsts '((a b c) (+ -) (1 2 3 4)))
(combine-list-of-lsts '((a b c) (+ -) (1 2) (o p)))
