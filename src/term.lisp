(defpackage :coffee.acupof.lambda-star.term
  (:use :common-lisp
	:optima)
  (:shadow :abs :push :pop))

(in-package :coffee.acupof.lambda-star.term)

(defstruct app
  level fun arg)

(defstruct name
  level str)

(defun name= (a b)
  (and (string= (name-str a) (name-str b))
       (= (name-level a) (name-level b))))

(defstruct var
  name skip sub)

(defstruct abs
  bind body)

(defstruct push
  name term)

(defstruct pop
  name)

;;; printer
(defun stringfy-name (name)
  (with-output-to-string (out)
    (format out "~a~a" (name-str name) (name-level name))))

(defun stringfy-term (term)
  (with-output-to-string (out)
    (match term
      ((var- (name (name- level str))
	     skip sub)
       (format out "~a~a^~a[~a]" str level skip (stringfy-subst sub)))
      ((app- level fun arg)
       (format out "~a @~a ~a"
	       (stringfy-term fun)
	       level
	       (stringfy-term arg)))
      ((abs (bind (name- level str)) body)
       (format out "\\~a~a.~a" str level (stringfy-term body))))))

(defun stringfy-subst (sub)
  (with-output-to-string (out)
    (mapc #'(lambda (x)
	      (match x
		((push- name term)
		 (format out "+~a(~a)" (stringfy-name name) (stringfy-term term)))
		((pop- name)
		 (format out "-~a" (stringfy-name name)))))
	  sub)))

(defun print-term (term)
  (princ (stringfy-term term)))

;;; parser
;; TERM := (var NAME SKIP SUB)
;;      |  (LEVEL TERM TERM)
;;      |  (fn NAME TERM)
;; SUB := ()
;;     |  ((+ NAME TERM) . SUB)
;;     |  ((- NAME) . SUB)
;; NAME := (STR LEVEL)

(defun parse (sexp)
  (match sexp
    ((list 'var name skip sub)
     (make-var :name (parse-name name) :skip skip :sub (parse-sub sub)))
    ((list 'fn name body)
     (make-abs :bind (parse-name name) :body (parse body)))
    ((list level fun arg)
     (make-app :level level :fun (parse fun) :arg (parse arg)))))

(defun parse-name (sexp)
  (make-name :str (car sexp) :level (cadr sexp)))

(defun parse-sub (sexp)
  (mapcar #'(lambda (x)
	      (match x
		((list '+ name term)
		 (make-push :name (parse-name name)
			    :term (parse term)))
		((list '- name)
		 (make-pop :name (parse-name name)))))
	  sexp))

;;;; Substitution
;;; <v, d>-component
(defun component (sub name% skip)
  (if (null sub)
      (make-var :name name% :skip skip :sub sub)
      (match (first sub)
	((push- name term)
	 (if (and (name= name name%) (zerop skip))
	     term
	     (component (rest sub) name% (- skip (if (equal name name%) 1 0)))))
	((pop- name)
	 (component (rest sub) name% (+ skip (if (name= name name%) 1 0)))))))

;;; S-restriction
(defun restrict (sub pred)
  (remove-if-not pred sub
		 :key (lambda (x) (match x
				    ((push- name) name)
				    ((pop- name) name)))))

(defun restrict< (sub level)
  (restrict sub (lambda (x) (< (name-level x) level))))

(defun restrict>= (sub level)
  (restrict sub (lambda (x) (>= (name-level x) level))))

;;; apply substitution
(defun apply-subst (term sub%)
  (if (null sub%)
      term
      (match term
	((var- name skip sub) (if (and (eq :id sub)
				       (eq :id (restrict>= sub% (name-level name))))
				  (make-var :name name
					    :skip skip
					    :sub sub%)
				  (apply-subst (component sub% name skip)
					       (restrict< (compose-subst sub sub%)
							  (name-level name)))))
	((abs- bind body)
	 (make-abs :bind bind
		   :body (apply-subst body
				      (cons (make-push :name bind
						       :term (make-var :name bind
								       :skip 0
								       :sub nil))
					    (compose-subst sub%
							   (list (make-pop :name bind)))))))
	((app- level fun arg)
	 (make-app :level level
		   :fun (apply-subst fun sub%)
		   :arg (apply-subst arg (restrict>= sub% level)))))))

;;; subst composition
(defun compose-subst (a b)
  (nconc (mapcar #'(lambda (x)
		     (match x
		       ((push- name term)
			(make-push :name name
				   :term (apply-subst term (restrict>= b (name-level name)))))
		       ((pop- ) x)))
		 a) b))

;;;; Reduction
(defun is-beta-redex (term)
  (match term
    ((guard (app- level
		  (fun (abs- (bind (name- (level level%))))))
	    (= level level%))
     t)
    (_ nil)))

(defun beta-reduce (term)
  (match term
    ((app- (fun (abs- bind body))
	   arg)
     (apply-subst body
		  (list (make-push :name bind :term arg))))))

;;;; eta-reduction
(defun subst-names (sub)
  (remove-duplicates (mapcar #'(lambda (x)
				 (match x
				   ((pop- name) name)
				   ((push- name) name)))
			     sub)
		     :test #'name=))

(defun restrict-name (sub name)
  (restrict sub #'(lambda (x) (name= name (match x
					    ((push- name) name)
					    ((pop- name) name))))))

(defun epsilon-reduce-1-able (sub)
  (match sub
    ((list* (pop- ) (push- ) _) t)
    ((cons _ rest) (epsilon-reduce-1-able rest))
    (nil nil)))

(defun epsilon-reduce-1 (sub)
  (match sub
    ((list* (pop- ) (push- ) rest) rest)
    ((cons x rest) (cons x (epsilon-reduce-1 rest)))
    (nil nil)))

(defun epsilon-reduce-2-able% (sub)
  (match sub
    ((cons (push- (name x) (term (var- (name y) skip (sub nil))))
	   rest)
     (and (name= x y)
	  (= (1- skip) (length rest))
	  (every (lambda (x) (and (pop-p x) (name= x (pop-name x))))
		 rest)))
    (_ nil)))

(defun epsilon-reduce-2% (sub)
  (cddr sub))

(defun epsilon-reduce-2-able (sub)
  (match sub
    ((guard x (epsilon-reduce-2-able% x)) t)
    ((cons _ rest) (epsilon-reduce-2-able rest))
    (t nil)))

(defun epsilon-reduce-2 (sub)
  (match sub
    ((guard x (epsilon-reduce-2-able% x)) (epsilon-reduce-2% x))
    ((cons x rest) (cons x (epsilon-reduce-2-able rest)))
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
