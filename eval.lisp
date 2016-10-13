(defpackage :lambda-star.evaluation
    (:use :common-lisp
	  :optima
	  :lambda-star.term
	  :lambda-star.reduction)
    (:export :reduce-term-leftmost
	     :reduce-subst-leftmost))

(in-package :lambda-star.evaluation)

;;; Left-most reduction
(defun reduce-term-leftmost (term)
  (if (is-beta-redex term)
      (values (beta-reduce term) t)
      (match term
	((var- name skip sub)
	 (multiple-value-bind (sub% win) (reduce-subst-leftmost sub)
	   (values (make-var :name name :skip skip :sub sub%) win)))
	((app- level fun arg)
	 (multiple-value-bind (fun% win) (reduce-term-leftmost fun)
	   (if win
	       (values (make-app :level level :fun fun% :arg arg) t)
	       (multiple-value-bind (arg% win) (reduce-term-leftmost arg)
		 (values (make-app :level level :fun fun :arg arg%) win)))))
	((abst- bind body)
	 (multiple-value-bind (body% win) (reduce-term-leftmost body)
	   (values (make-abst :bind bind :body body%) win))))))

(defun reduce-subst-leftmost (subst)
  (match subst
    (nil (values nil nil))
    ((list* (and pohe (spush name term)) rest)
     (multiple-value-bind (term% win) (reduce-term-leftmost term)
       (if win
	   (list* (make-spush :name name :term term%) rest)
	   (list* pohe (reduce-subst-leftmost rest)))))
    ((list* (and pohe (spop- )) rest)
     (list* pohe (reduce-subst-leftmost rest)))))
