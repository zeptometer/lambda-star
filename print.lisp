(defpackage :lambda-star.print
  (:use :common-lisp
	:optima
	:lambda-star.term))

(in-package :lambda-star.print)

(defmethod print-object ((obj name) stream)
  (match obj
    ((name- level str)
     (cond ((and (= level 0)
		 (every #'lower-case-p str))
	    (format stream "~a" str))
	   ((and (= level 1)
		 (every #'upper-case-p str))
	    (format stream "~a" str))
	   (t (format stream "~a~a" str level))))))

(defmethod print-object ((obj app) stream)
  (match obj
    ((app- level fun arg)
     (case level
       (0 (format stream "(~a~a)" fun arg))
       (1 (format stream "(~a@~a)" fun arg))
       (otherwise (format stream "(~a@~a~a)" fun level arg))))))

(defmethod print-object ((obj var) stream)
  (match obj
    ((var- name skip sub)
     (format stream "~a" name)
     (unless (zerop skip) (format stream "^~a" skip))
     (unless (null sub) (format stream "[~{~a~}]" sub)))))

(defmethod print-object ((obj abst) stream)
  (match obj
    ((abst- bind body)
     (format stream "(\\~a.~a)" bind body))))

(defmethod print-object ((obj spush) stream)
  (match obj
    ((spush- name term)
     (format stream "+~a(~a)" name term))))

(defmethod print-object ((obj spop) stream)
  (match obj
    ((spop- name)
     (format stream "-~a" name))))
