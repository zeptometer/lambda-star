(defpackage :lambda-star.print
  (:use :common-lisp
	:optima
	:lambda-star.term))

(in-package :lambda-star.print)

(defmethod print-unreadable-object ((obj name) stream)
  (match obj
    ((name- level str)
     (format stream "~a~a" str level))))

(defmethod print-unreadable-object ((obj app) stream)
  (match obj
    ((app- level fun arg)
     (format stream "(~a @~a ~a)" fun level arg))))

(defmethod print-unreadable-object ((obj var) stream)
  (match obj
    ((var- name skip sub)
     (format stream "~a^~a[~{~a ~}]" name skip sub))))

(defmethod print-unreadable-object ((obj abst) stream)
  (match obj
    ((abst- bind body)
     (format stream "(\\~a.~a)" bind body))))

(defmethod print-unreadable-object ((obj spush) stream)
  (match obj
    ((spush- name term)
     (format stream "+~a(~a)" name term))))

(defmethod print-unreadable-object ((obj spop) stream)
  (match obj
    ((spop- name)
     (format stream "-~a" name))))
