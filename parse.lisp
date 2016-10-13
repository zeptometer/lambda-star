(defpackage :lambda-star.parse
  (:use :common-lisp
	:optima
	:lambda-star.term)
  (:export :parse
	   :fn
	   :var))

(in-package :lambda-star.parse)

;;; parser
;; TERM := VAR
;;      |  (LEVEL TERM TERM)
;;      |  (fn NAME TERM)
;; VAR := NAME
;;      | (var NAME SKIP? SUB?)
;; SUB := ()
;;     |  ((+ NAME TERM) . SUB)
;;     |  ((- NAME) . SUB)
;; NAME := (STR LEVEL) | (a...z)* | (A...Z)*
;; SKIP := <non-negative number>
;; LEVEL := <non-negative number>

(defun parse (sexp)
  (match sexp
    ((or (var- ) (abst- ) (app- )) sexp)
    ((list 'fn name body)
     (make-abst :bind (parse-name name) :body (parse body)))
    ((guard (list level fun arg) (numberp level))
     (make-app :level level :fun (parse fun) :arg (parse arg)))
    ((guard x (symbolp x))
     (make-var :name (parse-name x) :skip 0 :sub nil))
    ((list 'var name)
     (make-var :name (parse-name name) :skip 0 :sub nil))
    ((list 'var name skip)
     (make-var :name (parse-name name) :skip skip :sub nil))
    ((list 'var name skip sub)
     (make-var :name (parse-name name) :skip skip :sub (parse-sub sub)))
    (_ (error "parse error"))))

(defun parse-name (sexp)
  (match sexp
    ((guard x (and (symbolp x)
		   (every #'upper-case-p (symbol-name x))))
     (make-name :str (symbol-name x) :level 0))
    ((guard x (and (symbolp x)
		   (every #'lower-case-p (symbol-name x))))
     (make-name :str (symbol-name x) :level 1))
    ((list name level)
     (make-name :str name :level level))
    (_ (error "parse erorr"))))

(defun parse-sub (sexp)
  (mapcar #'(lambda (x)
	      (match x
		((list '+ name term)
		 (make-spush :name (parse-name name)
			    :term (parse term)))
		((list '- name)
		 (make-spop :name (parse-name name)))))
	  sexp))
