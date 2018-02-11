;; 3.1

(defun combine-elt-lst (elt lst)
  (if (or (null lst) (not(atom elt)))
      nil
    (mapcar #'(lambda (x) (list elt x)) lst)))

(combine-elt-lst 'a nil) ;; --> NIL
(combine-elt-lst 'a '(1 2 3)) ;; --> ((A 1) (A 2) (A 3))

;; 3.2

(defun combine-lst-lst-rec (lst1 lst2)
  (if ( null (first lst1))
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