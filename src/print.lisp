(defpackage :coffee.acupof.lambda-star.print
  (:use :common-lisp
	:optima
	:coffee.acupof.lambda-star.term))

(in-package :coffee.acupof.lambda-star.print)

(defmethod print-object ((obj name) stream)
  (match obj
    ((name- level str)
     (format stream "~a~a" str level))))

(defmethod print-object ((obj app) stream)
  (match obj
    ((app- level fun arg)
     (format stream "(~a @~a ~a)" fun level arg))))

(defmethod print-object ((obj var) stream)
  (match obj
    ((var- name skip sub)
     (format stream "~a^~a[~{~a~}~^ ]" name skip sub))))

(defmethod print-object ((obj abst) stream)
  (match obj
    ((abst- bind body)
     (format stream "\~a.~a" bind body))))

(defmethod print-object ((obj spush) stream)
  (match obj
    ((spush- name term)
     (format stream "+~a(~a)" name term))))

(defmethod print-object ((obj spop) stream)
  (match obj
    ((spop- name)
     (format stream "-~a" name))))
