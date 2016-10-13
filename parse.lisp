(defpackage :lambda-star.parse
  (:use :common-lisp
	:optima
	:lambda-star.term)
  (:export :parse
	   :fn
	   :var))

(in-package :lambda-star.parse)

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
     (make-abst :bind (parse-name name) :body (parse body)))
    ((guard (list level fun arg) (numberp level))
     (make-app :level level :fun (parse fun) :arg (parse arg)))
    (_ (error "parse error"))))

(defun parse-name (sexp)
  (make-name :str (car sexp) :level (cadr sexp)))

(defun parse-sub (sexp)
  (mapcar #'(lambda (x)
	      (match x
		((list '+ name term)
		 (make-spush :name (parse-name name)
			    :term (parse term)))
		((list '- name)
		 (make-spop :name (parse-name name)))))
	  sexp))
