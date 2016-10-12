(defpackage :coffee.acupof.lambda-star.reduction
  (:use :common-lisp
	:optima
	:coffee.acupof.lambda-star.term)
  (:export :is-beta-redex
	   :beta-reduce
	   :normalize-subst))

(in-package :coffee.acupof.lambda-star.reduction)

;;;; Operations around substitution
;;; <v, d>-component
(defun component (sub name% skip)
  (if (null sub)
      (make-var :name name% :skip skip :sub sub)
      (match (first sub)
	((spush- name term)
	 (if (and (name= name name%) (zerop skip))
	     term
	     (component (rest sub) name% (- skip (if (name= name name%) 1 0)))))
	((spop- name)
	 (component (rest sub) name% (+ skip (if (name= name name%) 1 0)))))))

;;; S-restriction
(defun restrict (sub pred)
  (remove-if-not pred sub
		 :key (lambda (x) (match x
				    ((spush name) name)
				    ((spop- name) name)))))

(defun restrict< (sub level)
  (restrict sub (lambda (x) (< (name-level x) level))))

(defun restrict>= (sub level)
  (restrict sub (lambda (x) (>= (name-level x) level))))

;;; apply substitution
(defun apply-subst (term sub%)
  (if (null sub%)
      term
      (match term
	((var- name skip sub) (if (and (null sub)
				       (null (restrict>= sub% (name-level name))))
				  (make-var :name name
					    :skip skip
					    :sub sub%)
				  (apply-subst (component sub% name skip)
					       (restrict< (compose-subst sub sub%)
							  (name-level name)))))
	((abst- bind body)
	 (make-abst :bind bind
		   :body (apply-subst body
				      (cons (make-spush :name bind
						       :term (make-var :name bind
								       :skip 0
								       :sub nil))
					    (compose-subst sub%
							   (list (make-spop :name bind)))))))
	((app- level fun arg)
	 (make-app :level level
		   :fun (apply-subst fun sub%)
		   :arg (apply-subst arg (restrict>= sub% level)))))))

;;; subst composition
(defun compose-subst (a b)
  (nconc (mapcar #'(lambda (x)
		     (match x
		       ((spush name term)
			(make-spush :name name
				   :term (apply-subst term (restrict>= b (name-level name)))))
		       ((spop- ) x)))
		 a)
	 b))

;;;; Reduction, Convertion
;;; beta-reduction
(defun is-beta-redex (term)
  (match term
    ((guard (app- level
		  (fun (abst- (bind (name- (level level%))))))
	    (= level level%))
     t)
    (_ nil)))

(defun beta-reduce (term)
  (match term
    ((app- (fun (abst- bind body))
	   arg)
     (apply-subst body
		  (list (make-spush :name bind :term arg))))))

;;; epsilon-reduction
(defun subst-names (sub)
  (remove-duplicates (mapcar #'(lambda (x)
				 (match x
				   ((spop- name) name)
				   ((spush name) name)))
			     sub)
		     :test #'name=))

(defun restrict-name (sub name)
  (restrict sub #'(lambda (x) (name= name (match x
					    ((spush name) name)
					    ((spop- name) name))))))

(defun epsilon-reduce-1-able (sub)
  (match sub
    ((list* (spop- ) (spush ) _) t)
    ((list* _ rest) (epsilon-reduce-1-able rest))
    (nil nil)))

(defun epsilon-reduce-1 (sub)
  (match sub
    ((list* (spop- ) (spush ) rest) rest)
    ((list* x rest) (cons x (epsilon-reduce-1 rest)))
    (nil nil)))

(defun epsilon-reduce-2-able% (sub)
  (match sub
    ((guard (cons (spush (name x) (term (var- (name y) skip (sub nil))))
		  rest)
	    (and (name= x y)
		 (= (1- skip) (length rest))
		 (every (lambda (x) (and (spop-p x) (name= x (spop-name x))))
			rest)))
     t)
    (_ nil)))

(defun epsilon-reduce-2% (sub)
  (cddr sub))

(defun epsilon-reduce-2-able (sub)
  (match sub
    ((guard x (epsilon-reduce-2-able% x)) t)
    ((list* _ rest) (epsilon-reduce-2-able rest))
    (t nil)))

(defun epsilon-reduce-2 (sub)
  (match sub
    ((guard x (epsilon-reduce-2-able% x)) (epsilon-reduce-2% x))
    ((list* x rest) (cons x (epsilon-reduce-2-able rest)))
    (t sub)))

(defun normalize-subst% (sub)
  (cond ((epsilon-reduce-1-able sub)
	 (normalize-subst% (epsilon-reduce-1 sub)))
	((epsilon-reduce-2-able sub)
	 (normalize-subst% (epsilon-reduce-2 sub)))
	(t sub)))

(defun normalize-subst (sub)
  (reduce #'nconc
	  (mapcar #'normalize-subst%
		  (mapcar #'(lambda (x) (restrict-name sub x))
			  (subst-names sub)))))
